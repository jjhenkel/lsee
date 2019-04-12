import sys

THRESHOLD = 3
WORD_COUNTS = {}

all_lines = []
for line in sys.stdin:
  parts = list(filter(None,
    [ word.strip() for word in line.split(' ') ]
  ))
  fname = parts[0]
  all_lines.append((fname, parts[1:]))
  for word in parts[1:]:
    if word not in WORD_COUNTS:
      WORD_COUNTS[word] = set()
    WORD_COUNTS[word].add(fname)

FINAL_SET = set()
for word in WORD_COUNTS.keys():
  if len(WORD_COUNTS[word]) >= THRESHOLD:
    FINAL_SET.add(word)

for fname, parts in all_lines:
  remaining_parts = list(filter(lambda x: x in FINAL_SET, parts))
  if len(remaining_parts) <= 2: # Just $START $END
    continue
  print('{} {}'.format(fname, ' '.join(remaining_parts)))
