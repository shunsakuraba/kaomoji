import json
import sys

problems = json.loads(sys.stdin.read())

problems = sorted(problems, key=lambda problem: problem['size'])

for problem in problems:
    #if problem['size'] == 4:
        print problem
