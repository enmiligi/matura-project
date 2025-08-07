#include <errno.h>
#include <gc/gc.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/cdefs.h>

extern int64_t IMP_main();

int getErrno() { return errno; }

typedef struct String {
  bool isNil;
  struct String *contents[2];
} String;

typedef union StringContents {
  String *array[2];
  struct {
    char c;
    String *next;
  } actual;
} StringContents;

typedef struct Option {
  bool isNone;
  int64_t contents[1];
} Option;

typedef struct Void {
} Void;

void IMP_parseInt(String *string, Option *result) {
  String rest = *string;
  size_t length = 0;
  while (!rest.isNil) {
    length += 1;
    StringContents contents;
    contents.array[0] = rest.contents[0];
    contents.array[1] = rest.contents[1];
    rest = *contents.actual.next;
  }

  char text[length + 1];
  text[length] = 0;
  char *curr = &text[0];

  rest = *string;
  while (!rest.isNil) {
    StringContents contents;
    contents.array[0] = rest.contents[0];
    contents.array[1] = rest.contents[1];
    *curr = contents.actual.c;
    rest = *contents.actual.next;
    curr++;
  }

  int64_t i;
  char c;
  int scanned = sscanf(&text[0], "%" SCNd64 "%c", &i, &c);

  if (length == 0 || scanned != 1 || errno != 0) {
    result->isNone = true;
  } else {
    result->isNone = false;
    result->contents[0] = i;
  }
}

void IMP_parseFloat(String *string, Option *result) {
  String rest = *string;
  size_t length = 0;
  while (!rest.isNil) {
    length += 1;
    StringContents contents;
    contents.array[0] = rest.contents[0];
    contents.array[1] = rest.contents[1];
    rest = *contents.actual.next;
  }

  char text[length + 1];
  text[length] = 0;
  char *curr = &text[0];

  rest = *string;
  while (!rest.isNil) {
    StringContents contents;
    contents.array[0] = rest.contents[0];
    contents.array[1] = rest.contents[1];
    *curr = contents.actual.c;
    rest = *contents.actual.next;
    curr++;
  }

  double d;
  char c;

  int scanned = sscanf(&text[0], "%lf%c", &d, &c);

  if (scanned != 1 || errno != 0) {
    result->isNone = true;
  } else {
    result->isNone = false;
    result->contents[0] = *(int64_t *)(&d);
  }
}

void IMP_print(String *string, Void *result) {
  String rest = *string;
  size_t length = 0;
  while (!rest.isNil) {
    length += 1;
    StringContents contents;
    contents.array[0] = rest.contents[0];
    contents.array[1] = rest.contents[1];
    rest = *contents.actual.next;
  }

  char text[length + 1];
  text[length] = 0;
  char *curr = &text[0];

  rest = *string;
  while (!rest.isNil) {
    StringContents contents;
    contents.array[0] = rest.contents[0];
    contents.array[1] = rest.contents[1];
    *curr = contents.actual.c;
    rest = *contents.actual.next;
    curr++;
  }

  printf("%s", text);
}

String IMP_fromArray(char *array) {
  String s;
  s.isNil = true;

  ssize_t length = 0;
  for (ssize_t i = 0;; i++) {
    if (array[i] == 0) {
      length = i;
      break;
    }
  }

  for (ssize_t i = 0; i < length; i++) {
    ssize_t reverseIndex = length - i - 1;
    String *next = malloc(sizeof(String));
    *next = s;
    s.isNil = false;
    StringContents contents;
    contents.actual.c = array[reverseIndex];
    contents.actual.next = next;
    s.contents[0] = contents.array[0];
    s.contents[1] = contents.array[1];
  }

  return s;
}

void IMP_read(Void *v, String *result) {
  char *line = NULL;
  size_t lineCap = 0;
  ssize_t linelen;

  linelen = getline(&line, &lineCap, stdin);
  // Eliminate newline from line
  line[linelen - 1] = 0;

  *result = IMP_fromArray(line);

  free(line);
}

void IMP_showInt(int64_t *i, String *result) {
  int length = snprintf(NULL, 0, "%" PRId64, *i);
  char str[length + 1];

  snprintf(&str[0], length + 1, "%" PRId64, *i);

  *result = IMP_fromArray(&str[0]);
}

void IMP_showFloat(double *f, String *result) {
  int length = snprintf(NULL, 0, "%f", *f);
  char str[length + 1];

  snprintf(&str[0], length + 1, "%f", *f);

  *result = IMP_fromArray(&str[0]);
}

__attribute__((__malloc__())) void *IMP_malloc(size_t size) {
  return GC_MALLOC(size);
}

int main() { return IMP_main(); }
