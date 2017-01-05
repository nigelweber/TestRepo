import java.io.IOException;
import java.util.LinkedList;

public class Parser{

    // Return some data structure containing forced partial assignments.
    public static int[] parseForcedAssignments(LinkedList<String> in) throws InvalidMachineTaskException, PartialAssignmentErrorException{
        int[] result = new int[8];
        for(int i = 0; i < 8; i++){
            result[i] = -1;
        }

        int machine;
        int task;

        for(String line : in){
            String[] split = line.substring(1, line.length() - 1).split(",");
            try
            {
            	machine = Integer.parseInt(split[0])-1;
            }
            catch (Exception e){
            	throw new InvalidMachineTaskException();
            }
            task = getTaskNumber(split[1]);
            
            if(machine>7 || machine<0) throw new InvalidMachineTaskException();
            if(task>7 || task<0) throw new InvalidMachineTaskException();
            if(result[machine] != -1) throw new PartialAssignmentErrorException();
          
            result[machine] = task;
        }

        return result;
    }
    
    // Return a map ((machine, task) -> boolean) containing forbidden machines.
    public static boolean[][] parseForbiddenMachines(LinkedList<String> in) throws InvalidMachineTaskException{
        boolean[][] result = new boolean[8][8];
        int machine;
        int task;
        for(String line : in){
            String[] split = line.substring(1, line.length() - 1).split(",");
            try
            {
            	machine = Integer.parseInt(split[0])-1 ;
            }
            catch (Exception e)
            {
            	throw new InvalidMachineTaskException();
            }
            task = getTaskNumber(split[1]);
            
            /* Catch errors. */
            if(task<0 || task>7) throw new InvalidMachineTaskException();

            result[machine][task] = true;
        }
    	
    return result;
    }
    
    // Return some data structure containing Too-Near tasks.
    public static boolean[][] parseTooNearTasks(LinkedList<String> in) throws InvalidMachineTaskException{
        boolean[][] result = new boolean[8][8];
        int task1;
        int task2;
        for(String line : in){
            String[] split = line.substring(1, line.length() - 1).split(",");
            task1 = getTaskNumber(split[0]);
            task2 = getTaskNumber(split[1]);
            if(task2 < 0 || task1 < 0) throw new InvalidMachineTaskException();
            result[task1][task2] = true;
        }

        return result;
	}
	
    // Return a 2D matrix of penalties.
    public static long[][] parseMachinePenalties(LinkedList<String> in) throws InvalidPenaltyException, MachinePenaltyException{
        long[][] result = new long[8][8];

        /* Check input length. */
        if (in.size() != 8) throw new MachinePenaltyException();

        int i = 0;
        for(String line : in){
            String[] split = line.split(" ");
            
            /* Check input length. */
            if(split.length != 8) throw new MachinePenaltyException();

            for(int j = 0; j < 8; j++){
                try{
                    result[i][j] = Long.parseLong(split[j]);
                    if (result[i][j] < 0){
                    	throw new InvalidPenaltyException();
                    }
                } catch (NumberFormatException e){
                    throw new InvalidPenaltyException();
                }
            }

            i++;
        }
        return result;
    }
    // Returns an 8x8 matrix of pentalties, where [x][y] is the penalty for having y run on the machine next to x
    public static long[][] parseTooNearPenalties(LinkedList<String> in) throws InvalidPenaltyException, InvalidTaskException{
        long[][] result = new long[8][8];
        int sp = 0;
        int machine;
        int task;
        int task1;
        int task2;
        long value;
        for(String line : in){
            String[] split = line.substring(1, line.length() - 1).split(",");
            task1 = getTaskNumber(split[0]);
            task2 = getTaskNumber(split[1]);
            
            if(task1 < 0 || task2 < 0) throw new InvalidTaskException();
            try{
                value = Long.parseLong(split[2]);
            } catch (NumberFormatException e){
                throw new InvalidPenaltyException();
            }

            result[task1][task2] = value;
        }

        return result;
    }

    public static int getTaskNumber(String task){
        task = task.toLowerCase();
        if(task.equals("a")) return 0;
        if(task.equals("b")) return 1;
        if(task.equals("c")) return 2;
        if(task.equals("d")) return 3;
        if(task.equals("e")) return 4;
        if(task.equals("f")) return 5;
        if(task.equals("g")) return 6;
        if(task.equals("h")) return 7;
        return -1;
    }

	public static String getTaskLetter(int t) {
        switch(t){
        case 0:
            return "A";
        case 1:
            return "B";
        case 2:
            return "C";
        case 3:
            return "D";
        case 4:
            return "E";
        case 5:
            return "F";
        case 6:
            return "G";
        case 7:
            return "H";
        default:
            return "";
        }
	}
}
