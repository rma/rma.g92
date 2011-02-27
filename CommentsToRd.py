#!/usr/bin/python

"""This script creates Rd documentation from the comments in R source files.

   USAGE: CommentsToRd.py -r path -o path

   OPTIONS
       -r PATH
           The path containing the R source files.
           Default value: './R'.

       -o PATH
           The path where the Rd documentation will be stored.
           Default value: './man'.

       -h, --help
           Display this help message.

"""

import collections
import getopt
import glob
import itertools
import os
import os.path
import re
import sys

example_comment = """
#
# Fits a linear model to a set of observations. Blah blah blah.
#
# Args:
#   data:    The data frame that contains the model observations.
#   formula: The formula for the linear model.
#   step:    Select a reduced model by AIC (default is FALSE).
#   family:  The error distribution and link function (default is gaussian).
#
# Returns:
#   A list containing the fitted linear model (glm.all.Xs), and the reduced
#   model (glm.step.Xs) if step is TRUE. Both models inherit from the "glm"
#   class; see the documentation for "glm" for further details.
#
MakeGLM <- function(data, formula, step=FALSE, family=gaussian()) {
"""

example_output = """
\name{MakeGLM}
\alias{MakeGLM}
\title{Fits a linear model to a set of observations.}
\usage{
MakeGLM(data, formula, step=FALSE, family=gaussian())
}
\description{
  Fits a linear model to a set of observations. Blah blah blah.
}
\value{
  A list containing the fitted linear model (glm.all.Xs), and the reduced
  model (glm.step.Xs) if step is TRUE. Both models inherit from the "glm"
  class; see the documentation for "glm" for further details.
}
\arguments{
  \item{formula}{
    The formula for the linear model.
  }
  \item{step}{
    Select a reduced model by AIC (default is FALSE).
  }
  \item{data}{
    The data frame that contains the model observations.
  }
  \item{family}{
    The error distribution and link function (default is gaussian).
  }
}
"""

def strip_empty_lines((name, title_indent, content_indent, lines)):
    while len(lines) > 0 and lines[0].strip() == '':
        lines = lines[1:]
    while len(lines) > 0 and lines[-1].strip() == '':
        lines = lines[:-1]
    return (name, title_indent, content_indent, lines)

def first_sentence(lines):
    lines = " ".join(lines)
    period_re = re.compile('\\.(\\s|$)')
    m = period_re.search(lines)
    if m:
        return lines[:m.start() + 1]
    else:
        return lines

def parse_blocks(lines, last_ix):
    block_title_re = re.compile('^([a-zA-Z0-9\\.]+): (.*)$')
    block_title_only_re = re.compile('^([a-zA-Z0-9\\.]+):$')

    block_list = []

    first_ix = last_ix
    curr_line = lines[first_ix].strip()
    while len(curr_line) > 0 and curr_line[0] == '#':
        first_ix -= 1
        curr_line = lines[first_ix].strip()
    first_ix += 1

    for i in range(first_ix, last_ix + 1):
        curr_line = lines[i].strip()[1:]
        curr_text = curr_line.lstrip()
        indent_level = len(curr_line) - len(curr_text)

        m = block_title_re.match(curr_text)
        m = m if m else block_title_only_re.match(curr_text)
        if m:
            # new, named block begins here
            try:
                init_lines = [m.group(2).strip()]
            except IndexError:
                init_lines = []
            block_list.append((m.group(1), indent_level, None, init_lines))
        elif len(block_list) == 0:
            # new, un-named block begins here
            block_list.append(('', indent_level, None, [curr_text]))
        else:
            # continue reading previous block
            if block_list[-1][2] is None:
                (name, ttl_in, txt_in, block_lines) = block_list[-1]
                block_list[-1] = (name, ttl_in, indent_level, block_lines)
            line_indent = block_list[-1][2]
            if len(curr_text) > 0 and line_indent > indent_level:
                print "INDENT ERROR:", curr_text
            block_list[-1][3].append(curr_line[line_indent:])

        i -= 1
        curr_line = lines[i].strip()

    block_list = map(strip_empty_lines, block_list)

    return block_list

def get_block_dict(blocks, min_indent=0, ret_list=False):
    if blocks[0][0] == '':
        block_dict = {'': (blocks[0][3], {})}
        block_dict['.sorted_keys'] = ['']
        blocks = blocks[1:]
    else:
        block_dict = {}
        block_dict['.sorted_keys'] = []
        
    if len(blocks) == 0:
        if ret_list:
            return (block_dict, blocks)
        else:
            return block_dict

    prev_block = blocks[0]
    blocks = blocks[1:]

    block_dict[prev_block[0]] = (prev_block[3], {})
    block_dict['.sorted_keys'].append(prev_block[0])

    while len(blocks) > 0:
        curr_block = blocks[0]

        if curr_block[1] > prev_block[1]:
            ind = curr_block[1]
            (chn, blocks) = get_block_dict(blocks, min_indent=ind, ret_list=True)
            block_dict[prev_block[0]][1].update(chn)
        elif curr_block[1] < min_indent:
            break
        else:
            prev_block = curr_block
            block_dict[prev_block[0]] = (prev_block[3], {})
            block_dict['.sorted_keys'].append(prev_block[0])
            blocks = blocks[1:]

    if ret_list:
        return (block_dict, blocks)
    else:
        return block_dict

def write_Rd_block(f, blocks, block_key, block_name, dots=False, children=False):
    if not blocks.has_key(block_key):
        return
    if children and len(blocks[block_key][1]) == 0:
        return

    f.write('\\%s{\n' % (block_name))

    if children:
        #for name, desc in blocks[block_key][1].iteritems():
        for name in blocks[block_key][1]['.sorted_keys']:
            desc = blocks[block_key][1][name]
            if dots:
                name = name.replace('...', '\\dots')
            f.write('  \\item{%s}{\n' % (name,))
            for line in desc[0]:
                f.write('    %s\n' % (line,))
            f.write('  }\n')
    else:
        for line in blocks[block_key][0]:
            if dots:
                line = line.replace('...', '\\dots')
            f.write('  %s\n' % (line,))
    f.write('}\n')

def write_Rd(fn_file, fn_name, fn_usage, blocks):
    with open(fn_file, 'w') as f:
        f.write('\\name{%s}\n' % (fn_name,))
        f.write('\\alias{%s}\n' % (fn_name,))
        if blocks.has_key(''):
            f.write('\\title{%s}\n' % (first_sentence(blocks[''][0]),))
        else:
            f.write('\\title{%s}\n' % (fn_name,))
        f.write('\\usage{\n')
        # TODO -- keep usage as muliple lines
        f.write('%s\n' % (fn_usage.replace('...', '\\dots'),))
        f.write('}\n')
        write_Rd_block(f, blocks, '', 'description')
        write_Rd_block(f, blocks, 'Returns', 'value')
        # TODO -- arguments are not displayed in order
        write_Rd_block(f, blocks, 'Args', 'arguments', dots=True, children=True)
        write_Rd_block(f, blocks, 'Example', 'examples', dots=True)

def get_usage(lines, ix, start_from, fn_name):
    this_line = lines[ix][start_from:].strip()
    usage_str = this_line[0]

    if usage_str != "(":
        print("ERROR: first character after function is '%s'" % (usage_str,))
        print("       %s" % (fn_name,))
        return ""

    bracket_count = 1
    curr_pos = 1

    while True:
      next_ch = this_line[curr_pos]
      if next_ch == "(":
        bracket_count += 1
      elif next_ch == ")":
        bracket_count -= 1
      usage_str += next_ch
      if bracket_count == 0:
        break
      curr_pos += 1
      if curr_pos >= len(this_line):
        ix += 1
        this_line = lines[ix].strip()
        curr_pos = 0
        usage_str += "\n"
        
    return fn_name + usage_str

def create_doc(src_file, out_dir):
    regex = re.compile(r'^([\.a-zA-Z]+) <- function')
    with open(src_file, 'r') as f:
        lines = f.readlines()
        for (ix, line) in enumerate(lines):
            m = regex.match(line)
            if m:
                fn_name = m.group(1)
                fn_usage = get_usage(lines, ix, m.end(), fn_name)
                fn_file = os.path.join(out_dir, fn_name + ".Rd")
                blocks = parse_blocks(lines, ix - 1)
                blocks = get_block_dict(blocks)
                write_Rd(fn_file, fn_name, fn_usage, blocks)

def create_docs(src_dir, out_dir):
    out_dir = os.path.abspath(out_dir)
    src_dir = os.path.abspath(src_dir)

    old_wd = os.getcwd()
    os.chdir(src_dir)
    for src_file in glob.glob("*.[rR]"):
        src_file = os.path.join(src_dir, src_file)
        create_doc(src_file, out_dir)
    os.chdir(old_wd)

class Usage(Exception):
    """This exception is raised when invalid command-line arguments are given,
    or when the user passes '-h' or '--help' on the command-line."""
    def __init__(self, msg):
        self.msg = msg

def main(argv=None):
    """The main() function processes the command-line arguments and then calls
    create_docs() in order to create the Rd documentation files."""
    if argv is None:
        argv = sys.argv
    try:
        try:
            opts, args = getopt.getopt(argv[1:], "hr:o:", ["help"])
        except getopt.error, msg:
             raise Usage(msg)

        src_dir = './R'
        out_dir = './man'

        for o, a in opts:
            if o in ("-h", "--help"):
                print __doc__
                return 2
            elif o in ("-r"):
                src_dir = a
            elif o in ("-o"):
                out_dir = a

        if len(args) != 0:
            print __doc__
            return 2

        create_docs(src_dir, out_dir)

    except Usage, err:
        print >>sys.stderr, err.msg
        print >>sys.stderr, "For help use --help"
        return 2

if __name__ == "__main__":
    sys.exit(main())
