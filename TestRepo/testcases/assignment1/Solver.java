public class Solver{
  public static Node solve(int[] forced, boolean[][] forbidden, boolean[][] tooNear, long[][] penalties, long[][] tooNearPenalties) throws Exception{
    Node n = setupRoot(forced, forbidden, tooNear, penalties, tooNearPenalties);
    return solve(n,null,forbidden,tooNear);
  }
  
  private static Node solve(Node n, Node bestNode, boolean[][] forbidden, boolean[][] tooNear) throws Exception{
    // Check if the number of assigned tasks for n is 8.
    if(n.getTaskCount() >= 8){
      // There are no more empty slots, get the penalty value for this configuration.
      bestNode = n;
    } else {
      // Find a free machine.
      int m = n.getFreeMachine();
      
      // Create a branch for each remaining task.
      for(int t : n.getRemainingTasks()){
        
        // Check if the task assignment is forbidden.
      	int mPrev = m - 1 > -1 ? m - 1 : 7;
        int mNext = m + 1 < 8  ? m + 1 : 0;
        boolean isTooNear = n.getTask(mPrev) != -1 && tooNear[n.getTask(mPrev)][t];
        isTooNear = isTooNear || (n.getTask(mNext) != -1 && tooNear[t][n.getTask(mNext)]);

        if(!isTooNear && !forbidden[m][t])
        {
          // Create the new node and assign the task.
          Node next = new Node(n);
          next.setTask(m, t);
          
          // Check if the subtree rooted at the new node
          // has a smaller penalty than the current best.
          if(bestNode == null || next.getPenalty() < bestNode.getPenalty()){
            Node nextBestNode = solve(next, bestNode, forbidden, tooNear);
            long nextBest = nextBestNode == null ? Long.MAX_VALUE : nextBestNode.getPenalty();
            if(bestNode == null || nextBest < bestNode.getPenalty()){
              bestNode = nextBestNode;
            }
          }
        }
      }
    }
    return bestNode;
  }

  public static Node setupRoot(
    int [] forced,
    boolean[][] forbidden,
    boolean[][] tooNearTask,
    long[][] penalties,
    long[][] tooNearPenalties) throws Exception
  {
      int[] tasks = new int[8];
      
      // Initialize tasks.
      for(int i = 0; i < 8; i++){
        tasks[i] = -1;
      }

      for(int i = 0; i < 8; i++){
        int task = forced[i];
        if(task == -1) continue;
      
        if(tasks[i] == -1){
          tasks[i] = task;
        } else {
          throw new NoValidSolutionException();
        }

        //Check if Forbidden
        if(forbidden[i][task]){
          throw new NoValidSolutionException();
        }
        
        //Checking for too near task
        int prev = i == 0 ? 7 : i - 1;
        int next = i == 7 ? 0 : i + 1;
        
        //Check for i-1 to i
        if(tasks[prev] >=0 && tasks[prev] < 8){
          int prevTask = tasks[prev];
          if(tooNearTask[prevTask][task]){
            throw new NoValidSolutionException();
          }
        }
        
        //Check for i to 1+1
        if(tasks[next]!=-1){
          int nextTask = tasks[next];
          if(tooNearTask[task][nextTask]){
            throw new NoValidSolutionException();
          }
        }
      }
      return new Node(tasks, penalties, tooNearPenalties);
  }
}
