#include <assert.h>
#include <ncurses.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define HEIGHT 10
#define WIDTH 20

char _grid[HEIGHT][WIDTH];

#define BOMB 1
#define VISIBLE 2
#define FLAG 4
#define IS_SAFE(i, j) !(_grid[i][j] & BOMB)
#define IS_VISIBLE(i, j) (_grid[i][j] & VISIBLE)
#define IS_FLAGGED(i, j) (_grid[i][j] & FLAG)
#define FOR_EACH_NEIGHBOR(i, j, ni, nj) \
  for (ni = i - 1; ni <= i + 1; ni++) \
    for (nj = j - 1; nj <= j + 1; nj++) \
      if ( \
        ni >= 0 && ni < HEIGHT && \
        nj >= 0 && nj < WIDTH && \
        (ni != i || nj != j) \
      )

/** Initialize grid, creating `n` bombs at random. */
int init(int n) {
  if (n < 0 || n >= HEIGHT * WIDTH) {
    return -1;
  }

  srand(time(NULL));
  while (n) {
    int i = rand() % HEIGHT;
    int j = rand() % WIDTH;
    if (IS_SAFE(i, j)) {
      _grid[i][j] = BOMB;
      n--;
    }
  }
  return 0;
}

/** Get count of bombs adjacent to position. */
int neighboring_bombs(int i, int j) {
  int count = 0;
  int ni, nj;
  FOR_EACH_NEIGHBOR(i, j, ni, nj) {
    if (!IS_SAFE(ni, nj)) {
      count++;
    }
  }
  return count;
}

/** Reveal position, expanding if necessary (if count is 0). */
void reveal(int i, int j) {
  assert(i >= 0 && i < HEIGHT && j >= 0 && j < WIDTH);

  _grid[i][j] |= VISIBLE;
  if (!neighboring_bombs(i, j)) {
    int ni, nj;
    FOR_EACH_NEIGHBOR(i, j, ni, nj) {
      if (!IS_VISIBLE(ni, nj)) {
        reveal(ni, nj);
      }
    }
  }
}

/** Mark / unmark position as containing a bomb. */
int toggle_flag(int i, int j) {
  assert(i >= 0 && i < HEIGHT && j >= 0 && j < WIDTH);

  _grid[i][j] ^= FLAG;
  return IS_FLAGGED(i, j) ? 1 : -1;
}

/**
 * Check status of game for win / loss.
 *
 * Game ends successfully when all safe positions have been discovered, and in
 * a loss if a bomb has been discovered.
 */
int check(void) {
  int remaining = 0;

  int i, j;
  for (i = 0; i < HEIGHT; i++) {
    for (j = 0; j < WIDTH; j++) {
      if (!IS_SAFE(i, j)) {
        if (IS_VISIBLE(i, j)) {
          return -1; // Loss.
        }
        if (!IS_FLAGGED(i, j)) {
          remaining++;
        }
      }
    }
  }
  return remaining;
}

/** Display grid. */
void show(void) {
  int i, j;
  clear();
  for (i = 0; i < HEIGHT; i++) {
    for (j = 0; j < WIDTH; j++) {
      if (IS_FLAGGED(i, j)) {
        printw("F");
      } else if (IS_VISIBLE(i, j) && IS_SAFE(i, j)) {
        int n = neighboring_bombs(i, j);
        if (n) {
          printw("%d", n);
        } else {
          printw(" ");
        }
      } else {
        printw("%c", IS_VISIBLE(i, j) ? 'X' : '?');
      }
    }
    printw("\n");
  }
  refresh();
}

/** Main loop. */
int main(int argc, char **argv) {
  if (argc != 2) {
    fprintf(stderr, "usage: ./bin/main BOMBS\n");
    return -1;
  }

  int n = (int) atol(argv[1]);
  if (init(n)) {
    fprintf(stderr, "invalid number of bombs\n");
    return -1;
  }

  initscr();
  keypad(stdscr, TRUE);
  noecho();

  int ci = 0;
  int cj = 0;
  int flagged = 0;
  char *msg = NULL;
  while (1) {
    show();
    move(ci, cj);
    switch (getch()) {
    case 'k':
    case KEY_UP:
      if (ci > 0) {
        ci--;
      }
      break;
    case 'j':
    case KEY_DOWN:
      if (ci < HEIGHT - 1) {
        ci++;
      }
      break;
    case 'h':
    case KEY_LEFT:
      if (cj > 0) {
        cj--;
      }
      break;
    case 'l':
    case KEY_RIGHT:
      if (cj < WIDTH - 1) {
        cj++;
      }
      break;
    case 'f':
      if (flagged < n || IS_FLAGGED(ci, cj)) {
        flagged += toggle_flag(ci, cj);
      }
      break;
    case 'd':
      reveal(ci, cj);
      break;
    case 'q':
      goto quit;
      break;
    }
    switch (check()) {
    case -1:
      msg = "boom :(";
      goto quit;
    case 0:
      msg = "congrats!";
      goto quit;
    }
  }

quit:
  endwin();
  echo();
  if (msg != NULL) {
    printf("%s\n", msg);
  }
  return 0;
}
