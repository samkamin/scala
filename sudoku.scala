import scala.collection.mutable.Set

def mset (s : scala.collection.immutable.Set[Int]) : Set[Int] = { return Set[Int]() ++ s; }
val sq0 = Set(0, 1, 2, 9, 10, 11, 18, 19, 20);
val hor0 = mset((0 to 8).toSet);
val vert0 = mset((0 to 8).map(x => x * 9).toSet);

def incr (s : Set[Int], i : Int) : Set[Int] = {
   return s.map((x : Int) => x + i);
}

val squares = List(0, 3, 6, 27, 27+3, 27+6, 45, 45+3, 54+6).map(x => incr(sq0, x));
val hors = (0 to 8).map(x => incr(hor0, x * 9));
val verts = (0 to 8).map(x => incr(vert0, x));

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

val grid = puzz1.map(x => if (x == 0) (0 to 8).toSet else Set(x)).toArray;

// given list of cells, return all elements in those cells
def elements (s : Set[Int]) : Set[Int] = {
   var r = Set[Int]();
   s.foreach(i =>r ++= grid(i));
   return r;
}

// active_grid is subset of grid still "in play" - i.e. not a singleton
// for squares, hors, verts, the "active_" prefix is a bit misleading:
// all squares, hors, verts are "active", and these are lists of the same
// length as squares, etc., but they contain only active cells.
var active_grid = (0 to 80).filter (i => grid(i).size > 1).toSet;
var active_squares = squares.map(s => s intersect active_grid).toSet;
var active_hors = hors.map(s => s intersect active_grid).toSet;
var active_verts = verts.map(s => s intersect active_grid).toSet;

def print_grid () = {
   for (i <- 0 to 8) {
      for (j <- 0 to 8) {
         print(grid(i * 9 + j) + "   ")
      }
      println();
   }
}
print_grid();

def filter_groups () = {
   active_squares = squares.map(s => s intersect active_grid).toSet;
   active_hors = hors.map(s => s intersect active_grid).toSet;
   active_verts = verts.map(s => s intersect active_grid).toSet;
}

def rule1 () = {
   for (g <- active_squares ++ active_hors ++ active_verts) {
      for ((h_hat, i_hat) <- subsets(g)) {
         var h_elts = elements(h_hat);
         if (h_elts.size == h_hat.size) {
            for (c <- i_hat) {
               grid(c) --= h_elts;
               if (grid(c).size == 1) {
                  active_grid -= c;
                  filter_groups();
               }
            }
         }
      }
   }
}

def subsets (s : Set[Int]) : Set[(Set[Int], Set[Int])] = {
   return Set((Set[Int](), Set[Int]()));
}
