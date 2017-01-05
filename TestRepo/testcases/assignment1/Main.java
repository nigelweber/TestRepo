import java.util.ArrayList;
import java.util.LinkedList;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.FileWriter;
import java.util.Dictionary;
import java.util.Scanner;
import java.io.FileReader;
import java.io.FileNotFoundException;


public class Main {

    static String[] _args;
    /** main method */
	public static void main(String[] args) {
		try{
			// Check if correct arguments are being used
			// Check if input file is valid
	        _args = args;
	        if(args.length != 2){
	            System.out.println("Usage: <input file> <output file>");
	            return;
	        }
	        Scanner in = new Scanner(new BufferedReader(new FileReader(args[0])));
	
	        // Retrieve name of input file
	        String line = rtrim(in.nextLine());
	        checkTitle(line, "Name:");
			String name = rtrim(in.nextLine()); // Get name.
		    
	        /* Skip all blank lines. */
	        line = skip(in);
	        
	        // Test for forced partial assignments
	        // Uses parseForcedAssignments(LinkedList <String>) from Parser
	        // If forced partial assignment found, write error message to output file 
	        checkTitle(line, "forced partial assignment:");
			int[] forcedAssignments = Parser.parseForcedAssignments(getLines(in));
		    for (int m=0; m<8; m++){
		    	if (forcedAssignments[m] != -1){
			    	for (int n=0; n<8; n++){
			    		if (n!=m && forcedAssignments[n] == forcedAssignments[m]){
			    			throw new PartialAssignmentErrorException();
			    		}
			    	}
		    	}
			}
	
	        line = skip(in);
	        
	        // Test for forbidden machines
	        // Uses parseForbiddenMachines(LinkedList<Strings>) from Parser
	        // If forbidden machine assignment found, write error message to file
	        checkTitle(line, "forbidden machine:");
			boolean[][] forbiddenMachine = null;
	        forbiddenMachine = Parser.parseForbiddenMachines(getLines(in));
	        
	        line = skip(in);
	        
	        // Test for too-near tasks
	        // Uses parseTooNearTasks(LinkedList<Strings>) from Parser
	        // If too-near tasks assignments are found, write error message to file
	        checkTitle(line, "too-near tasks:");
			boolean[][] tooNear = Parser.parseTooNearTasks(getLines(in));
	
	        line = skip(in);
	        
	        // Get machine penalties and apply penalties
	        // Uses parseMachinePenalties(LinkedList<Strings>) from Parser
	        // If error, write error to output file
	        checkTitle(line, "machine penalties:");       
			long[][] machinePenalties = Parser.parseMachinePenalties(getLines(in));
	
	        line = skip(in);
	        
	        // Get too-near penalties and apply penalties
	        // Uses parseTooNearPenalties(LinkedList<Strings>) from Parser
	        // If error, write error to output file
	        checkTitle(line, "too-near penalities");
			long[][] tooNearPenalties = Parser.parseTooNearPenalties(getLines(in));
			
			while(in.hasNext()){
				if(!in.nextLine().trim().equals("")) throw new ParseException();
			}
			
	        Node result;
	        result = Solver.solve(forcedAssignments, forbiddenMachine, tooNear, machinePenalties, tooNearPenalties);
	        
            if(result != null){
	            WriteToFile(result.toString(), _args[1]);
            } else {
                throw new NoValidSolutionException();
            }
		} catch (Exception e){
			WriteToFile(e.getMessage(), _args[1]);
		}
    }

    /** Check if the line matches the title. */
    private static boolean matches(String line, String match) throws ParseException{
        if(!line.equals(match)){
        	throw new ParseException();
        }
        return true;
    }
    
    private static void checkTitle(String line, String match) throws ParseException{
    	if(!line.equals(match)){
    		throw new ParseException();
    	}
    }
        
    /** Gets the next section of lines. */
    private static LinkedList<String> getLines(Scanner in){
        LinkedList<String> lines = new LinkedList<String>();
        String line;
        if(in.hasNext()){
        	line = rtrim(in.nextLine());
        	while(!line.equals("")){
        		lines.add(line);
        		if(in.hasNext()){
        			line = rtrim(in.nextLine());
        		} else {
        			line = "";
        		}
        	}
        }
        return lines;
    }

    /** Moves forward through the scanner until a non-empty line
     *  is found. */
    private static String skip(Scanner in) throws ParseException{
    	String line = null;
    	if(in.hasNext()){
    		line = in.nextLine();
    		if(line.equals("") && in.hasNext()){
    			line = in.nextLine();
    		}
    	}
    	if(line != null && !line.equals("")){
    		return line;
    	}
    	throw new ParseException();
    }

     /** Trim whitespace from only the right side of the string. 
      *  Start from end of the string
      *  Navigate to front of string until NO whitespace found
   	  *  Return the string consisting of the remaining characters
      */
    private static String rtrim(String s){
        if (s == null) {
        	return null;
        }
        int i = s.length() - 1;
        for(; i >= 0 && Character.isWhitespace(s.charAt(i)); i--);
        return s.substring(0, i + 1);
    }
	
	/** Method to write to file given a filename 
	 *  If file exists, create new file
	 *  Write string to file and add newline
	 *  Close the file
	 * 	Notify user of error if unable to write to file
	 */
	public static void WriteToFile(String inputStringToFile, String fileName) {
		try {
			File file = new File(fileName);
	 
			if (!file.exists()) {
				file.createNewFile();
			}
 
			FileWriter fw = new FileWriter(file.getAbsoluteFile(),false); //false to overwrite, true to append
			BufferedWriter buffWriter = new BufferedWriter(fw);
			buffWriter.write(inputStringToFile);
			buffWriter.newLine();
			buffWriter.close();
 
			System.out.println(inputStringToFile);
			System.out.println("Done writing to file.");
	 
			} catch (IOException e) {
				System.out.println("Error writing to file");
			}
	}
}
