package amazed.solver;

import amazed.maze.Maze;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.concurrent.ConcurrentSkipListSet;

/**
 * <code>ForkJoinSolver</code> implements a solver for
 * <code>Maze</code> objects using a fork/join multi-thread
 * depth-first search.
 * <p>
 * Instances of <code>ForkJoinSolver</code> should be run by a
 * <code>ForkJoinPool</code> object.
 */


public class ForkJoinSolver
    extends SequentialSolver
{
    private boolean hasFoundGoal = false;
    public ArrayList<ForkJoinSolver> forklist = new ArrayList<>();
    /**
     * Creates a solver that searches in <code>maze</code> from the
     * start node to a goal.
     *
     * @param maze   the maze to be searched
     */
    public ForkJoinSolver(Maze maze)
    {
        super(maze);
        this.visited = new ConcurrentSkipListSet<>();
    }

    /**
     * Creates a solver that searches in <code>maze</code> from the
     * start node to a goal, forking after a given number of visited
     * nodes.
     *
     * @param maze        the maze to be searched
     * @param forkAfter   the number of steps (visited nodes) after
     *                    which a parallel task is forked; if
     *                    <code>forkAfter &lt;= 0</code> the solver never
     *                    forks new tasks
     */
    public ForkJoinSolver(Maze maze, int forkAfter)
    {
        this(maze);
        this.forkAfter = forkAfter;
    }

   /**
   * Creates a solver that searches in <code>maze</code> from the
   * start node to a goal.
   * initialises the start node, visited set and hasFoundGoal
   * @param maze   the maze to be searched
   * @param start    start node
   * @param visited   visited set of nodes
   * @param hasFound  hasFoundGoal the termination condition
   */
    public ForkJoinSolver(Maze maze, int start, Set<Integer> visited, boolean hasFound)
    {
        this(maze);
        this.start = start;
        this.visited = visited;
        this.hasFoundGoal = hasFound;
    }

    /**
     * Searches for and returns the path, as a list of node
     * identifiers, that goes from the start node to a goal node in
     * the maze. If such a path cannot be found (because there are no
     * goals, or all goals are unreacheable), the method returns
     * <code>null</code>.
     *
     * @return   the list of node identifiers from the start node to a
     *           goal node in the maze; <code>null</code> if such a path cannot
     *           be found.
     */
    @Override
    public List<Integer> compute()
    {
        return parallelSearch();
    }

    private List<Integer> parallelSearch()
    {
       // one player active on the maze at start
       int player = maze.newPlayer(start);
       frontier.push(start);
       while (!hasFoundGoal & !frontier.isEmpty()) {
           System.err.println("1");
           int current = frontier.pop();
           // mark node as visited
           maze.move(player, current);
           visited.add(current);
           System.err.println("2");
           // check if current node is a goal
           if (maze.hasGoal(current)) {
               hasFoundGoal = true;
               return pathFromTo(start, current);
           }
           for (int nb : maze.neighbors(current)) {
               if (!visited.contains(nb)) {
                   System.err.println("3");
                   frontier.push(nb);
                   ForkJoinSolver fjs = new ForkJoinSolver(maze, nb, visited, hasFoundGoal);
                   forklist.add(fjs);
                   fjs.fork();
               }
           }
       }
       for (ForkJoinSolver fjs : forklist) {
           List<Integer> path = fjs.join();
           if (path != null) {
               return path;
           }
            // all nodes explored, no goal found
        }
            return null;
    }
}
