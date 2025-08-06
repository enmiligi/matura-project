#include <errno.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

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

void parseInt(String *string, Option *result) {
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

void parseFloat(String *string, Option *result) {
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

void print(String *string, Void *result) {
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
  }

  printf("%zu%s\n", length, text);
}

String fromArray(char *array) {
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

void read(Void *v, String *result) {
  fflush(stdout);
  char *line = NULL;
  size_t lineCap = 0;
  ssize_t linelen;

  linelen = getline(&line, &lineCap, stdin);

  *result = fromArray(line);

  free(line);
}

void showInt(int64_t *i, String *result) {
  int length = snprintf(NULL, 0, "%" PRId64, *i);
  char str[length + 1];

  snprintf(&str[0], length + 1, "%" PRId64, *i);

  *result = fromArray(&str[0]);
}

void showFloat(double *f, String *result) {
  int length = snprintf(NULL, 0, "%f", *f);
  char str[length + 1];

  snprintf(&str[0], length + 1, "%f", *f);

  *result = fromArray(&str[0]);
}
