# Zombie solver via SMT
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

INPUT_PER_ORACLE = 1
EXTENDED_INPUT = INPUT_PER_ORACLE
ZEROLINE = EXTENDED_INPUT
ONELINE = EXTENDED_INPUT + 1

#NORACLE = int(sys.stdin.next().striip())

#oracle_inputs = [long(x, 0) for x in sys.stdin.next().split()]
#oracle_outputs = [long(x, 0) for x in sys.stdin.next().split()]

ORACLES = []
#ORACLES = [ (oracle_inputs[i], oracle_outputs[i]) for i in range(NORACLE)]
IN = 0
OUT = INPUT_PER_ORACLE

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
for i in range(LIB_LINES):
    if lineops[i] == "if0":
        # if0 (const) e e
        S.add(LI[i][0] != ZEROLINE)
        S.add(LI[i][0] != ONELINE)
        S.add(LI[i][1] != LI[i][2])
        # no op reuse => strict orderling enabled
        S.add(Implies(LI[i][0] > ONELINE, And(LI[i][0] < LI[i][1],
                                              LI[i][0] < LI[i][2])))
        S.add(Implies(LI[i][1] > ONELINE, LI[i][1] < LI[i][2]))
        S.add(Not(And(LI[i][0] == 0, # input
                      LI[i][1] == 0, # input
                      LI[i][2] == ZEROLINE)))
    elif lineops[i] in ["shr1", "shr4", "shr16"]:
        # equivalent to 0
        S.add(LI[i][0] != ZEROLINE)
        S.add(LI[i][0] != ONELINE)

        for j in range(LIB_LINES):
            if ((lineops[i], lineops[j]) in
                [("shr1", "shr4"),
                 ("shr1", "shr16"),
                 ("shr4", "shr16")]):
                S.add(LI[i][0] != LO[j]) # (shr1 (shr4 ..)) is eqiv. to (shr4 (shr1 ..))
    elif lineops[i] == "shl1":
        # equivalent to 0
        S.add(LI[i][0] != ZEROLINE)

    elif lineops[i] in ["and", "or", "xor", "plus"]:
        # equivalent to 0 or copy value
        S.add(LI[i][0] != ZEROLINE)
        S.add(LI[i][1] != ZEROLINE)

        # break symmetry
        if lineops[i] in ["and", "or", "xor"]:
            S.add(LI[i][0] < LI[i][1])
        else:
            S.add(LI[i][0] <= LI[i][1])
            S.add(Implies(LI[i][0] > ONELINE, 
                          LI[i][0] < LI[i][1]))

for i in range(LIB_LINES):
    for j in range(i + 1, LIB_LINES):
        if lineops[i] == lineops[j]:
            S.add(LO[i] < LO[j])
            if lineops[i] == "not":
                S.add(LI[i][0] != LO[j])
                S.add(LI[j][0] != LO[i])

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

        col_lib = []
        for (opuse, arity) in [(nuops, 0),
                               (unops, 1),
                               (binops, 2),
                               (triops, 3)]:
            for op in opuse:
                for opl in range(size_by_op[op]):
                    Icur = []
                    suffix = "_%i_%s_%i" % (nin, op, opl)
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

                    I[nin].append(Icur)
                    O[nin].append(Ocur)
        S.add(col_lib)

        col_conn = []
        for i in range(LIB_LINES):
            for j in range(ninputs[i]):
                for k in range(LIB_LINES):
                    col_conn.append(Implies(LI[i][j] == LO[k], I[nin][i][j] == O[nin][k]))
        for inp in range(INPUT_PER_ORACLE):
            for i in range(LIB_LINES):
                for j in range(ninputs[i]):
                    col_conn.append(Implies(LI[i][j] == inp,
                                            I[nin][i][j] == BitVecVal(ORACLES[nin][IN + inp], 64)))

        for i in range(LIB_LINES):
            col_conn.append(Implies(LO[i] == (EXTENDED_LINES - 1),
                                    O[nin][i] == BitVecVal(ORACLES[nin][OUT], 64)))

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
            if li < INPUT_PER_ORACLE:
                inelms.append("input")
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





