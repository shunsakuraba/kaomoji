# Zombie solver via SMT
# Why zombie? Cause It'll counter phoenix.
from z3 import *

import sys

if len(sys.argv) >= 2:
    cut_oracle = int(sys.argv[1])
else:
    cut_oracle = 9999

#nuops = '0 1'.split()
#unops = 'not shl1 shr1 shr4 shr16'.split()
#binops = 'and or xor plus'.split()
#triops = 'if0'.split()

#(unops_use, binops_use, triops_use) = (['shl1'], [], [])

ans = sys.stdin.next()
size = int(sys.stdin.next().strip())
unops_use = sys.stdin.next().split()
binops_use = sys.stdin.next().split()
triops_use = sys.stdin.next().split()

if "tfold" in triops_use or "fold" in triops_use:
    print >> sys.stderr, "Does not support fold / tfold yet"
    sys.exit(1)

oracle_inputs = [long(x, 0) for x in sys.stdin.next().split()]
oracle_outputs = [long(x, 0) for x in sys.stdin.next().split()]

INPUT_PER_ORACLE = 1
EXTENDED_INPUT = INPUT_PER_ORACLE + 2
MAXCMP = size - 1 - (len(unops_use) + len(binops_use) * 2 + len(triops_use) * 3 - 1)
NORACLE = len(oracle_inputs)
OP_KINDS = len(unops_use) + len(binops_use) + len(triops_use)
LIB_LINES = MAXCMP * OP_KINDS
EXTENDED_LINES = EXTENDED_INPUT + LIB_LINES

if NORACLE > cut_oracle:
    NORACLE = cut_oracle

ORACLES = [ (oracle_inputs[i], oracle_outputs[i]) for i in range(NORACLE)]
IN = 0
OUT = 1

#print ORACLES


lineops = []
ninputs = []
for opl in range(MAXCMP):
    for (opuse, arity) in [(unops_use, 1),
                           (binops_use, 2),
                           (triops_use, 3)]:
        for op in opuse:
            lineops.append(op)
            ninputs.append(arity)

LI = [[Int("LI_%i_%i" % (i, j)) for j in range(ninputs[i])] for i in range(LIB_LINES)]
col_li = [And(0 <= LI[i][j],
              LI[i][j] < EXTENDED_LINES)
          for i in range(LIB_LINES) for j in range(ninputs[i])]

LO = [Int("LO_%i" % i) for i in range(LIB_LINES)]
col_lo = [And(EXTENDED_INPUT <= LO[i],
              LO[i] < EXTENDED_LINES)
          for i in range(LIB_LINES)]

col_cons = [ Distinct(LO) ]
col_acyc = [ LI[i][j] < LO[i] for i in range(LIB_LINES) for j in range(ninputs[i])]

col_noreuse = []
for i in range(LIB_LINES):
    for j in range(ninputs[i]):
        for k in range(LIB_LINES):
            for l in range(ninputs[k]):
                if (i, j) == (k, l):
                    continue
                col_noreuse.append(Implies(LI[i][j] >= EXTENDED_INPUT,
                                           LI[i][j] != LI[k][l]))

col_lib = []

I = []
O = []
for nin in range(NORACLE):
    I.append([])
    O.append([])
    for opl in range(MAXCMP):
        for (opuse, arity) in [(unops_use, 1),
                               (binops_use, 2),
                               (triops_use, 3)]:
            for op in opuse:
                Icur = []
                suffix = "_%i_%s_%i" % (nin, op, opl)
                # print suffix
                for a in range(arity):
                    Icur.append(BitVec("I%s_%i" % (suffix, a), 64))
                Ocur = BitVec("O%s" % suffix, 64)
                if op == 'not':
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

col_conn = []
for nin in range(NORACLE):
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
        for j in range(ninputs[i]):
            col_conn.append(Implies(LI[i][j] == INPUT_PER_ORACLE + 0,
                                    I[nin][i][j] == BitVecVal(0, 64)))
            col_conn.append(Implies(LI[i][j] == INPUT_PER_ORACLE + 1,
                                    I[nin][i][j] == BitVecVal(1, 64)))

    for i in range(LIB_LINES):
        col_conn.append(Implies(LO[i] == (EXTENDED_LINES - 1),
                                O[nin][i] == BitVecVal(ORACLES[nin][OUT], 64)))

#print I
#print O

S = Solver()
S.add(col_li)
S.add(col_lo)
S.add(col_cons)
S.add(col_acyc)
S.add(col_noreuse)
S.add(col_lib)
S.add(col_conn)

#print S
print "SAT construction done. Start solving ..."

if S.check() != sat:
    print "Could not find solution"
    sys.exit(1)

m = S.model()

# out, i, op
lines = [("OUT", "IN[]", "op") for i in range(LIB_LINES)]

for i in range(LIB_LINES):
    inelms = [] 
    for j in range(ninputs[i]):
        li = m[LI[i][j]].as_long()
        if li < INPUT_PER_ORACLE:
            inelms.append("input")
        elif li < INPUT_PER_ORACLE + 2:
            inelms.append(str(li - INPUT_PER_ORACLE))
        else:
            inelms.append("x%d" % int(li - EXTENDED_INPUT))
    oute = m[LO[i]].as_long() - EXTENDED_INPUT
    ope = lineops[i]
    lines[i] = (oute, inelms, ope)

lines.sort()
for l in lines:
    print "x%d = %s(%s)" % (l[0], l[2], ",".join(l[1]))
    
print ans    

S.add(Not(LI[0][0] == 0))
S.check()
print S.model




