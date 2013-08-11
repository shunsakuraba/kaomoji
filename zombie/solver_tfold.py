# Zombie solver via SMT
# Which only works with Tfold.
# Why zombie? 'Cause It'll counter phoenix.
from z3 import *

import sys
import os
import threading

if len(sys.argv) >= 2:
    cut_oracle = int(sys.argv[1])
else:
    cut_oracle = 9999

nuops = '0 1'.split()
unops = 'not shl1 shr1 shr4 shr16'.split()
binops = 'and or xor plus'.split()
triops = 'if0'.split()

#(unops_use, binops_use, triops_use) = (['shl1'], [], [])

size = int(sys.stdin.readline().strip())
nopkinds = len(unops) + len(binops) + len(triops)
ops = [sys.stdin.readline().split() for _ in range(nopkinds)]
ops = [(op, int(n)) for [op, n] in ops]
size_by_op = {}
for (op, n) in ops:
    size_by_op[op] = n
size_by_op['0'] = 1
size_by_op['1'] = 1
print >> sys.stderr, size_by_op

#if "tfold" in triops_use or "fold" in triops_use:
#    print >> sys.stderr, "Does not support fold / tfold yet"
#    sys.exit(1)

print >> sys.stderr, "Initializing with size=%d (pid=%d)" % (size, os.getpid())
# print "Initializing with size=%d" % size
# sys.stdout.flush()

# tfold is (lambda (x) (fold x 0 (lambda (x y) e)))
# where y is accumulator (initially bound to 0)
# and x is input bytes
ACCUM = 0
BYTES = 1

INPUT_PER_ORACLE = 2
EXTENDED_INPUT = INPUT_PER_ORACLE
ZEROLINE = EXTENDED_INPUT
ONELINE = EXTENDED_INPUT + 1

#NORACLE = int(sys.stdin.next().striip())

#oracle_inputs = [long(x, 0) for x in sys.stdin.next().split()]
#oracle_outputs = [long(x, 0) for x in sys.stdin.next().split()]

ORACLES = []
#ORACLES = [ (oracle_inputs[i], oracle_outputs[i]) for i in range(NORACLE)]
IN = 0
OUT = 1

#print ORACLES

S = Solver()

lineops = []
ninputs = []
for (opuse, arity) in [(nuops, 0),
                       (unops, 1),
                       (binops, 2),
                       (triops, 3)]:
    for op in opuse:
        nopl = size_by_op[op]
        for opl in range(nopl):
            lineops.append(op)
            ninputs.append(arity)

LIB_LINES = len(ninputs)
EXTENDED_LINES = EXTENDED_INPUT + LIB_LINES

LI = [[Int("LI_%i_%i" % (i, j)) for j in range(ninputs[i])] for i in range(LIB_LINES)]
col_li = [And(0 <= LI[i][j],
              LI[i][j] < EXTENDED_LINES)
          for i in range(LIB_LINES) for j in range(ninputs[i])]
S.add(col_li)

LO = [Int("LO_%i" % i) for i in range(LIB_LINES)]
col_lo = [And(EXTENDED_INPUT <= LO[i],
              LO[i] < EXTENDED_LINES)
          for i in range(LIB_LINES)]
S.add(col_lo)
S.add(LO[0] == ZEROLINE) # 0
S.add(LO[1] == ONELINE) # 1

# Optimizations
opts = []
for i in range(LIB_LINES):
    if lineops[i] == "if0":
        # if0 (const) e e
        opts.append(LI[i][0] != ZEROLINE)
        opts.append(LI[i][0] != ONELINE)
        opts.append(LI[i][1] != LI[i][2])
        # no op reuse => strict orderling enabled
        opts.append(Implies(LI[i][0] > ONELINE, And(LI[i][0] < LI[i][1],
                                              LI[i][0] < LI[i][2])))
        opts.append(Implies(LI[i][1] > ONELINE, LI[i][1] < LI[i][2]))
    elif lineops[i] in ["shr1", "shr4", "shr16"]:
        # equivalent to 0
        opts.append(LI[i][0] != ZEROLINE)
        opts.append(LI[i][0] != ONELINE)

        for j in range(LIB_LINES):
            if ((lineops[i], lineops[j]) in
                [("shr1", "shr4"),
                 ("shr1", "shr16"),
                 ("shr4", "shr16")]):
                opts.append(LI[i][0] != LO[j]) # (shr1 (shr4 ..)) is eqiv. to (shr4 (shr1 ..))
    elif lineops[i] == "shl1":
        # equivalent to 0
        opts.append(LI[i][0] != ZEROLINE)

    elif lineops[i] in ["and", "or", "xor", "plus"]:
        # equivalent to 0 or copy value
        opts.append(LI[i][0] != ZEROLINE)
        opts.append(LI[i][1] != ZEROLINE)

        # break symmetry
        if lineops[i] in ["and", "or", "xor"]:
            opts.append(LI[i][0] < LI[i][1])
        else:
            opts.append(LI[i][0] <= LI[i][1])
            opts.append(Implies(LI[i][0] > ONELINE, 
                          LI[i][0] < LI[i][1]))

for i in range(LIB_LINES):
    for j in range(i + 1, LIB_LINES):
        if lineops[i] == lineops[j]:
            opts.append(LO[i] < LO[j])
            if lineops[i] == "not":
                opts.append(LI[i][0] != LO[j])
                opts.append(LI[j][0] != LO[i])
S.add(opts)

col_cons = [ Distinct(LO) ]
S.add(col_cons)

col_acyc = [ LI[i][j] < LO[i] for i in range(LIB_LINES) for j in range(ninputs[i])]
S.add(col_acyc)

col_noreuse = []
for i in range(LIB_LINES):
    for j in range(ninputs[i]):
        for k in range(LIB_LINES):
            for l in range(ninputs[k]):
                if (i, j) == (k, l):
                    continue
                col_noreuse.append(Implies(And(LI[i][j] >= EXTENDED_INPUT,
                                               LI[i][j] != ZEROLINE,
                                               LI[i][j] != ONELINE),
                                           LI[i][j] != LI[k][l]))
S.add(col_noreuse)

print "Initialize complete"
sys.stdout.flush()

I = []
O = []

nin = 0
while True:

    nline = sys.stdin.readline()
    if nline.strip() == "Q":
        break
    nline = int(nline)

    for _inc_oracle in range(nline):
        line = sys.stdin.readline().strip()
        print >> sys.stderr, "Incoming input: %s" % line

        # Add one oracle
        I.append([])
        O.append([])

        [oin, oout] = [long(x, 0) for x in line.split()]
        ORACLES.append((oin, oout))

        # Python does not have right-shift
        fold_inputs = [simplify(LShR(BitVecVal(oin, 64), i * 8) & BitVecVal(0xffL, 64)) for i in range(8)]
        print >> sys.stderr, fold_inputs

        col_lib = []
        for folditer in range(8):
            I[nin].append([])
            O[nin].append([])
            for (opuse, arity) in [(nuops, 0),
                                   (unops, 1),
                                   (binops, 2),
                                   (triops, 3)]:
                for op in opuse:
                    for opl in range(size_by_op[op]):
                        Icur = []
                        suffix = "_%i_%i_%s_%i" % (nin, folditer, op, opl)
                        # print suffix
                        for a in range(arity):
                            Icur.append(BitVec("I%s_%i" % (suffix, a), 64))
                        Ocur = BitVec("O%s" % suffix, 64)
                        if op == '0':
                            constr = (Ocur == BitVecVal(0, 64))
                        elif op == '1':
                            constr = (Ocur == BitVecVal(1, 64))
                        elif op == 'not':
                            constr = (Ocur == ~(Icur[0]))
                        elif op == 'shl1':
                            constr = (Ocur == Icur[0] << 1)
                        elif op == 'shr1':
                            constr = (Ocur == LShR(Icur[0], 1))
                        elif op == 'shr4':
                            constr = (Ocur == LShR(Icur[0], 4))
                        elif op == 'shr16':
                            constr = (Ocur == LShR(Icur[0], 16))
                        elif op == 'and':
                            constr = (Ocur == Icur[0] & Icur[1])
                        elif op == 'or':
                            constr = (Ocur == Icur[0] | Icur[1])
                        elif op == 'xor':
                            constr = (Ocur == Icur[0] ^ Icur[1])
                        elif op == 'plus':
                            constr = (Ocur == Icur[0] + Icur[1])
                        elif op == 'if0':
                            constr = (Or(And(Icur[0] == BitVecVal(0, 64),
                                             Ocur == Icur[1]),
                                         And(Icur[0] != BitVecVal(0, 64),
                                             Ocur == Icur[2])))
                        #print constr
                        col_lib.append(constr)

                        I[nin][folditer].append(Icur)
                        O[nin][folditer].append(Ocur)
        S.add(col_lib)

        accum_out = [BitVec('y_%i_%i' % (nin, fi), 64) for fi in range(8)]
        col_conn = []
        for i in range(LIB_LINES):
            for j in range(ninputs[i]):
                for k in range(LIB_LINES):
                    for folditer in range(8):
                        col_conn.append(Implies(LI[i][j] == LO[k],
                                                I[nin][folditer][i][j] == O[nin][folditer][k]))

        # Connect accum input 
        for i in range(LIB_LINES):
            for j in range(ninputs[i]):
                for folditer in range(8):
                    if folditer == 0:
                        # tfold's accumulator is 0 initialized
                        col_conn.append(Implies(LI[i][j] == ACCUM,
                                                I[nin][folditer][i][j] == BitVecVal(0, 64))) 
                    else:
                        # connect to accumulator output of previous loop
                        col_conn.append(Implies(LI[i][j] == ACCUM,
                                                I[nin][folditer][i][j] == accum_out[folditer - 1]))

                    col_conn.append(Implies(LI[i][j] == BYTES,
                                            I[nin][folditer][i][j] == fold_inputs[folditer]))


        for i in range(LIB_LINES):
            for folditer in range(8):
                col_conn.append(Implies(LO[i] == (EXTENDED_LINES - 1),
                                        O[nin][folditer][i] == accum_out[folditer]))

        col_conn.append(accum_out[7] == BitVecVal(ORACLES[nin][OUT], 64))
        #print >>sys.stderr, col_conn

        S.add(col_conn)
        nin += 1
    print >> sys.stderr, "Start Solving with %d oracles" % nin

    res = S.check()
    if res != sat:
        print >> sys.stderr, "Stopped reason =", res
        print "0\n"
        sys.stdout.flush()
        print >> sys.stderr, "Could not find solution"
        sys.exit(1)
        
    m = S.model()
    lines = [("OUT", "IN[]", "op") for i in range(LIB_LINES)]

    for i in range(LIB_LINES):
        inelms = [] 
        for j in range(ninputs[i]):
            li = m[LI[i][j]].as_long()
            if li == ACCUM:
                inelms.append("accum")
            elif li == BYTES:
                inelms.append("byte")
            else:
                inelms.append("x%d" % int(li - EXTENDED_INPUT))
        oute = m[LO[i]].as_long() - EXTENDED_INPUT
        ope = lineops[i]
        lines[i] = (oute, inelms, ope)

    lines.sort()
    print len(lines)
    print >> sys.stderr, len(lines)
    for l in lines:
        if l[1] == []:
            print "%d %s _" % (l[0], l[2])
        else:
            print "%d %s %s" % (l[0], l[2], ",".join(l[1]))
        print >> sys.stderr, "%d %s %s" % (l[0], l[2], ",".join(l[1]))
    sys.stdout.flush()





