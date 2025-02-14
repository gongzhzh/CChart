package amazed.solver;

import amazed.maze.Maze;
import java.util.List;
import java.util.Stack;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicReference;

/**
 * <code>ForkJoinSolver</code> implements a solver for
 * <code>Maze</code> objects using a fork/join multi-thread depth-first search.
 */
public class ForkJoinSolver extends SequentialSolver {

    // Shared flag to indicate when a goal has been found.
    private static final AtomicReference<List<Integer>> solutionPath = new AtomicReference<>(null);

    /**
     * Creates a solver that searches in <code>maze</code> from the start node to a goal.
     *
     * @param maze The maze to be searched.
     */
    public ForkJoinSolver(Maze maze) {
        super(maze);
        this.predecessor = new ConcurrentHashMap<>();
    }

    /**
     * Creates a solver that searches in <code>maze</code> from the start node to a goal,
     * forking after a given number of visited nodes.
     *
     * @param maze      The maze to be searched.
     * @param forkAfter The number of steps (visited nodes) after which a parallel task is forked.
     *                  If <code>forkAfter <= 0</code>, the solver never forks new tasks.
     */
    public ForkJoinSolver(Maze maze, int forkAfter) {
        this(maze);
        this.forkAfter = forkAfter;
    }

    @Override
    public List<Integer> compute() {
        System.out.println("ForkJoinSolver is being used!");
        return parallelSearch();
    }

    private int customStartNode = -1;

    private List<Integer> parallelSearch() {
        // If a forked task set a custom starting node, use it; otherwise, use the maze’s original start.
        int startNode = (customStartNode == -1) ? start : customStartNode;

        // **Stop early if a solution has already been found**
        if (solutionPath.get() != null) {
            return null;
        }

        // Create a player for animation starting at startNode.
        int player = maze.newPlayer(startNode);
        System.out.println("New player created with id: " + player + " at node: " + startNode);

        // Use a local stack to perform DFS (each task has its own stack).
        Stack<Integer> localFrontier = new Stack<>();
        localFrontier.push(startNode);

        int steps = 0; // Step counter for fork control.
        List<ForkJoinSolver> forkedTasks = new java.util.ArrayList<>();

        // Process nodes until there are none left in this task's stack.
        while (!localFrontier.empty()) {
            int current = localFrontier.pop();

            // Stop immediately if another thread has found the goal**
            if (solutionPath.get() != null) {
                return null;
            }

            // If the current node is a goal, move there and reconstruct the path.
            if (maze.hasGoal(current)) {
                System.out.println("Goal found at node: " + current + " by player " + player);
                maze.move(player, current);

                // Store the solution path atomically
                List<Integer> foundPath = pathFromTo(start, current);
                solutionPath.compareAndSet(null, foundPath); // Ensure only the first thread sets it

                return foundPath;
            }

            // Ensure that only one task processes this node**
            if (visited.add(current)) {
                System.out.println("Player " + player + " moving to node: " + current);
                maze.move(player, current);

                // Process all neighbors of the current node.
                for (int nb : maze.neighbors(current)) {
                    if (!visited.contains(nb)) {  // Check if it's unvisited before doing anything
                        predecessor.put(nb, current);

                        if (forkAfter > 0 && steps >= forkAfter) {
                            // Stop forking if the goal is already found
                            if (solutionPath.get() != null) {
                                return null;
                            }

                            System.out.println("Forking a new task from node: " + current + " for neighbor: " + nb);

                            ForkJoinSolver subtask = new ForkJoinSolver(maze, forkAfter);
                            subtask.visited = visited;
                            subtask.predecessor = predecessor;
                            subtask.start = start;
                            subtask.customStartNode = nb;
                            subtask.fork();
                            forkedTasks.add(subtask);
                        } else {
                            System.out.println("Player " + player + " exploring neighbor " + nb + " from node: " + current);
                            localFrontier.push(nb);
                        }
                    }
                }
                steps++; // Move this outside the loop so steps count properly.
            }
        }

        // Join forked tasks and check if any found a solution.
        for (ForkJoinSolver task : forkedTasks) {
            if (solutionPath.get() != null) {
                return solutionPath.get(); // Immediately return the found path
            }
            System.out.println("Joining a forked task.");
            List<Integer> result = task.join();
            if (result != null) {
                return result;
            }
        }

        // Ensure the solution path is returned if found
        return solutionPath.get();
    }
}
