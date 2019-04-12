import os
import sys
import json

data = []

for line in sys.stdin:
  data.append(
    [ word.strip() for word in line.split(' ')[1:] ]
  )


def tree_step(prefix, idx):
  choices = {}
  
  for trace in data:
    # Needs to match the word
    if trace[:idx] != prefix:
      continue
    # Don't go out of bounds
    if idx < len(trace):
      if trace[idx] not in choices:
        choices[trace[idx]] = 0
      choices[trace[idx]] += 1
  
  return choices


def build_tree(data):
  TREE = {}
  TREE['$START'] = {}

  maxlen = max([ len(trace) for trace in data ])

  workingset = [ (['$START'], 1) ]
  while len(workingset) > 0:
    current, idx = workingset.pop()

    choices = tree_step(current, idx)

    if len(choices.keys()) == 0:
      continue

    for choice in choices.keys():
      leaf = TREE
      for word in current:
        leaf = leaf[word]
      leaf[choice] = {}
      workingset.append((
        current + [ choice ], idx + 1
      ))

  return TREE


def get_tails(current, K):
  resultset = set()

  workingset = []
  workingset.append((current, K, []))
  
  while len(workingset) > 0:
    c, k, tail = workingset.pop()
    if k > 0 and len(c.keys()) > 0:
      for key in c.keys():
        workingset.append((c[key], k-1, tail + [ key ]))
    else:
      resultset.add(' '.join(tail))

  return '|'.join(resultset)


def build_graph(tree, K=1):
  vertices = {}
  edges = {}
  vidx = 1

  workingset = [ (0, tree['$START'], '$START') ]

  while len(workingset) > 0:
    p, c, n = workingset.pop()

    k_tails = get_tails(c, K)
    print(k_tails)

    if k_tails not in vertices:
      vertices[k_tails] = vidx
      vidx += 1

    if p is not None and (p, n, vertices[k_tails]) not in edges:
      edges[(p, n, vertices[k_tails])] = 0
    edges[(p, n, vertices[k_tails])] += 1
    
    for key in c.keys():
      workingset.append((
        vertices[k_tails], c[key], key
      ))

  # DOT
  print('%DOT digraph G {')
  for (a,b,c),_ in edges.items():
    print('%DOT {} -> {} [ label="{}"];'.format(a,c,b))
  print('%DOT }')
  print()

  # FSA
  # labels = set()
  # end_states = set()
  # for (a,b,c),_ in sorted(edges.items(), key=lambda x: x[0][0]):
  #   print('%FSA {} {} {}'.format(a,c,b))
  #   labels.add(b)
  #   if b == '$END':
  #     end_states.add(c)
  # for s in end_states:
  #   print('%FSA {}'.format(s))
  # print()

  # # Symbols
  # print('%SYM <eps> 0')
  # as_list = list(labels)
  # for i in range(0, len(as_list)):
  #   print('%SYM {} {}'.format(as_list[i], i+1))
  # print()

  # # One-Error FST
  # for l1 in labels:
  #   # Stay at these states and emit identity
  #   print('%FST 0 0 {} {}'.format(l1, l1))
  #   print('%FST 1 1 {} {}'.format(l1, l1))

  #   # Could delete word l1
  #   print('%FST 0 1 {} <eps>'.format(l1))
  #   print('%FST 1 0 {} <eps>'.format(l1))
  #   for l2 in labels:
  #     if l1 == l2:
  #       continue
  #     # Could re-write word l1 -> l2 for any l2
  #     print('%FST 0 1 {} {}'.format(l1, l2))
  #     print('%FST 1 0 {} {}'.format(l1, l2))
  # print('%FST 0')
  # print('%FST 1')

if __name__ == '__main__':
  tree = build_tree(data)
  print(json.dumps(tree, indent=2, sort_keys=True))
  build_graph(tree)
