import os
import sys
import math
import random

SAMPLES = 20
NO_SAMPLE = 'SAMPLE' in os.environ and (os.environ['SAMPLE'] == 'NONE')

def score(x, choices):
  score = 0.0
  for _, as_set in choices:
    score += 1.0 - (float(len(
      x[1].intersection(as_set)
    )) / float(len(
      x[1].union(as_set)
    )))  
  return score / float(len(choices))

# Get lines as a map from function to its traces
# represent as a list of tuples of strings and sets
lines_as_sets = {}
for line in sys.stdin:
  # No sampling
  if NO_SAMPLE:
    print(line.strip())
    continue

  parts = list(filter(None, [ word.strip() for word in line.split(' ') ]))
  fname = parts[0]

  if fname not in lines_as_sets:
    lines_as_sets[fname] = []

  as_set = set(parts[2:-1])
  if len(as_set) == 0:
    continue
  lines_as_sets[fname].append((line.strip(), as_set))


if 'SAMPLE' in os.environ:
  # Random sampling
  if os.environ['SAMPLE'] == 'RANDOM':
    for fname in lines_as_sets.keys():
      if len(lines_as_sets[fname]) <= SAMPLES:
        for trace, _ in lines_as_sets[fname]:
          print(trace)
      else:
        for i in range(SAMPLES):
          print(random.choice(lines_as_sets[fname])[0])
  # Diversity Sampling
  elif os.environ['SAMPLE'] == 'DIVERSITY':
    for fname in lines_as_sets.keys():
      if len(lines_as_sets[fname]) <= SAMPLES:
        for trace, _ in lines_as_sets[fname]:
          print(trace)
      else:
        choices = [ lines_as_sets[fname][0] ]
        while len(choices) < SAMPLES:
          furthest = list(sorted([ 
            (x, score(x, choices)) for x in lines_as_sets[fname]
            if x not in choices
          ], key=lambda x: -x[1]))

          if len(furthest) == 0:
            choices.append(random.choice(choices))
            continue

          choices.append(furthest[0][0])
        for trace, _ in choices:
          print(trace)

