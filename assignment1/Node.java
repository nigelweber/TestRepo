import java.util.Map;

public class Node{
    private static long[][] penalties;
    private static long[][] tooNearPenalties;

    private int[] tasks = new int[8];
    private boolean[] assigned = new boolean[8];

    public Node(int[] tasks, long[][] penalties, long[][] tooNearPenalties){
        this.penalties = penalties;
        this.tooNearPenalties = tooNearPenalties;
        this.tasks = tasks;
        for(int i : tasks){
        	if(i >=0 && i < 8) assigned[i] = true;
        }
    }

    public Node(Node parent){
        for(int i = 0; i < 8; i++){
            tasks[i] = parent.getTask(i);
            if(tasks[i] != -1) assigned[tasks[i]] = true;
        }
    }

    public void setTask(int machine, int task) throws Exception{
        if(machine < 0 || machine > 7) throw new Exception("invalid machine");
        if(task < 0 || task > 7) throw new Exception("invalid task");
        if(tasks[machine] != -1) throw new Exception("machine already assigned");
        if(assigned[task]) throw new Exception("task already assigned");
        assigned[task] = true;
        tasks[machine] = task;
    }
    
    public int getTask(int machine){
        return tasks[machine];
    }

    public long getPenalty(){
        long penalty = 0;
        for(int i = 0; i < 8; i++){
            if(tasks[i] != -1){
                penalty += penalties[i][tasks[i]];
                int iPrev = i - 1 > -1 ? i - 1 : 7;
                if(tasks[iPrev] != -1){
                    penalty += tooNearPenalties[tasks[iPrev]][tasks[i]];
                }
            }
        }
        return penalty;
    }

    public long getTaskCount(){
        long count = 0;
        for(int i = 0; i < 8; i++){
            if(tasks[i] != -1) count++;
        }
        return count;
    }
    
    public int[] getRemainingTasks(){
        // Count how many remaining tasks there are.
        int count = 0;
        for(int i = 0; i < 8; i++){
            if(tasks[i] == -1) count++;
        }

        // Create the result array of size count.
        int[] result = new int[count];

        // Fill the result array.
        for(int i = 0; i < 8; i++){
            if(!assigned[i]) result[--count] = i;
        }
        return result;
    }

    public int getFreeMachine(){
        int result = 0;
        while(tasks[result] != -1) result++;
        return result;
    }

    public String toString(){
        StringBuilder result = new StringBuilder();
        result.append("Solution ");
        int count = 7;
        for(int t : tasks){
        	result.append(Parser.getTaskLetter(t));
        	if(count-- > 0) result.append(" ");
        }
        result.append("; Quality: " + getPenalty());
        return result.toString();
    }
}
