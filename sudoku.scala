import scala.collection.mutable.Set
import util.control.Breaks._

def mset (s : scala.collection.immutable.Set[Int]) : Set[Int] = { return Set[Int]() ++ s; }

// 'location' - should be abstract; concretely, a number from 0 to 80
//    ('location' and 'cell' are sometimes used ambiguously)
// 'grid' - contents of all cells, i.e. map from location -> (non-empty) subset of {1,..,9}
var grid = (0 to 80).map(x => if (x == 0) (1 to 9).toSet else Set(x)).toArray;

def init_grid (l : List[Int]) = {
   grid = l.map(x => if (x == 0) (1 to 9).toSet else Set(x)).toArray;
}

def print_grid () = {
   println();
   for (i <- 0 to 8) {
      for (j <- 0 to 8) {
         for (k <- 1 to 9) {
            print(if (grid(i * 9 + j).contains(k)) k else " ");
         }
         if ((j + 1) % 3 == 0 && j != 8)
            print(" | ")
         else if (j < 8)
            print("   ");
      }
      println();
      if ((i + 1) % 3 == 0 && i != 8) {
         print("-" * 12 * 9)
         println();
      }
   }
}

// An 'active location' is a non-singleton cell in the grid
// 'square', 'row', 'col' - *fixed* set of nine locations
// 'active_square', 'active_row', 'active_col' - subset of a square, row, col
// 'squares' - array of nine squares (i.e. map from {0..8} -> square)
//    'rows' and 'cols' similarly
// 'active_squares' = array of nine active-square's
//    'active_rows' and 'active_cols' similarly
// TODO: these are tied to the concrete rep of locations; shouldn't be
val squares = {
   val sq0 = Set(0, 1, 2, 9, 10, 11, 18, 19, 20);
   List(0, 3, 6, 27, 27+3, 27+6, 54, 54+3, 54+6).map(x => incr(sq0, x)).toArray;
}
val rows = {
   val row0 = mset((0 to 8).toSet);
   (0 to 8).map(x => incr(row0, x * 9)).toArray;
}
val cols = {
   val col0 = mset((0 to 8).map(x => x * 9).toSet);
   (0 to 8).map(x => incr(col0, x)).toArray;
}
// will be assigned correct values in init
var active_squares = (0 to 8).map(x => Set[Int]()).toArray;
var active_rows = (0 to 8).map(x => Set[Int]()).toArray;
var active_cols = (0 to 8).map(x => Set[Int]()).toArray;

// c a cell (0 - 80), result a square (0 to 8); similarly for row_of, col_of
def square_of (c : Int) : Int = {
   var h = c / 27;
   var v = c % 9 / 3;
   h * 3 + v;
}
def row_of (c : Int) : Int = { c / 9; }
def col_of (c : Int) : Int = { c % 9; }

// active_grid is subset of grid still "in play" - i.e. not a singleton
// (not sure if this is needed)
var active_grid = (0 to 80).filter (i => grid(i).size > 1).toSet;

def incr (s : Set[Int], i : Int) : Set[Int] = {
   return s.map((x : Int) => x + i);
}
// given list of cells, return all elements in those cells
def elements (s : Set[Int]) : Set[Int] = {
   var r = Set[Int]();
   s.foreach(i => r ++= grid(i));
   r;
}

def is_active (c : Int) : Boolean = { return grid(c).size > 1; }

def is_solved () : Boolean = { (0 to 80).forall(c => ! is_active(c)); }

// group = square, row, or col.  Result is all singleton (i.e. final)
// values in that group
def finalvals (g : Set[Int]) : Set[Int] = {
   val h = g.filter(i => grid(i).size == 1);
   elements(h);
}

// adjust grid and actives, after making any change to grid
def filter_groups () = {
   for (i <- 0 to 80) {
      if (grid(i).size > 1) {
         val finals = finalvals(squares(square_of(i))) ++
                      finalvals(rows(row_of(i))) ++
                      finalvals(cols(col_of(i)));
         grid(i) --= finals;
      }
   }
   active_squares = (0 to 8).map(s => squares(s).filter(is_active)).toArray;
   active_rows = (0 to 8).map(s => rows(s).filter(is_active)).toArray;
   active_cols = (0 to 8).map(s => cols(s).filter(is_active)).toArray;
}

def init () { filter_groups(); }

// Rule 1: Within any group G, let G = H u I be a proper partition,
//         and E = elements(H).  If | E | = | H |, then for each
//         cell i in I, set grid(i) to grid(i) - E.
// Optimization: consider only active groups, i.e. with any active
//         group G, let G = H u I be a proper partition, where H and
//         I are active cells.  Etc.
def rule1 () : Int = {
   var changed = 0;
   for (g <- active_squares ++ active_rows ++ active_cols) {
      for (h_hat <- subsets2(g)) {
         var h_elts = elements(h_hat);
         if (h_elts.size == h_hat.size) {
            var i_hat = g.diff(h_hat);
            for (c <- i_hat) {
               val n = grid(c).size;
               grid(c) --= h_elts;
               val p = grid(c).size;
               changed += n - p;
               if (grid(c).size == 1) {
                  active_grid -= c;
                  filter_groups();
               }
            }
         }
      }
   }
   changed;
}

// Rule 2: For any row or column G, and any intersecting square S, let
//         GS = G inter S, Gs = G - S, Sg = S - G.
//   2A: Let E = elts(GS) - elts(Sg).  For every cell c in Gs, set grid(c) to grid(c) - E
//   2B: Let E = elts(GS) - elts(Gs).  For every cell c in Sg, set grid(c) to grid(c) - E
// Optimization: again confined this to active groups/squares.
def rule2 () : Int = {
   var changed = 0;
   for (g <- active_rows ++ active_cols) {
      for (s <- active_squares) {
         val GS = g & s;
         if (! GS.isEmpty) {
            val Gs = g &~ s;
            val Sg = s &~ g;
            {
               val E = elements(GS) &~ elements(Sg);
               if (! E.isEmpty) {
                  for (c <- Gs) {
                     var n = grid(c).size;
                     grid(c) --= E;
                     var p = grid(c).size;
                     changed += (n - p);
                  }
               }
            }
            {
               val E = elements(GS) &~ elements(Gs);
               if (! E.isEmpty) {
                  for (c <- Sg) {
                     var n = grid(c).size;
                     grid(c) --= E;
                     var p = grid(c).size;
                     changed += (n - p);
                  }
               }
            }
            filter_groups();
         }
      }
   }
   changed;
}

// all proper subsets of size >= 2
def subsets2 (s : Set[Int]) : Set[Set[Int]] = {
   // all subsets
   def subs2 (s : Set[Int]) : Set[Set[Int]] = {
      if (s.size == 0) return Set(s);
      val i = s.head;
      var S = subs2(s - i);
      var T = Set[Set[Int]]();
      T ++= S;
      for (t <- S) {
         T += (t + i);
      }
      return T;
   }
   var S = subs2(s);
   var T = Set[Set[Int]]();
   for (t <- S) {
      if (t.size >= 2 && t.size < s.size)
         T += t;
   }
   return T;
}

val puzz1 = List(
     0, 0, 2, 0, 0, 0, 0, 5, 9,
     8, 0, 0, 1, 0, 0, 0, 0, 0,
     4, 0, 0, 0, 0, 0, 0, 2, 3,
     0, 2, 0, 0, 0, 5, 0, 0, 0,
     0, 0, 0, 8, 0, 0, 0, 6, 1,
     0, 0, 7, 0, 0, 0, 0, 0, 0,
     0, 0, 0, 7, 0, 3, 0, 0, 0,
     0, 0, 0, 0, 0, 2, 9, 0, 6,
     3, 0, 0, 0, 9, 8, 0, 0, 0
);
val puzz2 = List(
     3, 1, 0, 2, 0, 0, 0, 0, 5,
     9, 7, 0, 4, 0, 0, 0, 6, 1,
     0, 4, 0, 1, 7, 6, 0, 0, 9,
     1, 0, 8, 0, 0, 2, 9, 3, 0,
     4, 0, 3, 0, 9, 0, 0, 0, 6,
     6, 0, 0, 0, 0, 3, 4, 8, 0,
     0, 0, 9, 0, 2, 0, 0, 0, 0,
     0, 6, 0, 0, 0, 8, 1, 7, 3,
     5, 0, 0, 7, 6, 0, 0, 0, 8
);
val puzz3 = List(
     0, 0, 0, 0, 0, 0, 7, 0, 8,
     7, 0, 0, 0, 0, 8, 0, 0, 6,
     0, 0, 0, 0, 5, 4, 0, 2, 1,
     0, 1, 0, 9, 4, 6, 3, 0, 0,
     0, 0, 0, 0, 0, 0, 0, 0, 0,
     0, 2, 4, 0, 0, 0, 0, 1, 0,
     0, 0, 3, 0, 6, 2, 0, 0, 0,
     0, 0, 6, 0, 7, 0, 0, 0, 4,
     0, 5, 0, 0, 0, 0, 0, 0, 0
);

init_grid(puzz3);
init();
print_grid();

var changed = 0;
do {
   changed = rule1() + rule2();
}
while (changed > 0);
print_grid();
println("Solved: " + is_solved());
