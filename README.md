# Set-Operations-api
Set Operations API

<h2>API functionallity</h2>

Currently implemented API functions are found within the `set-relation.R` file. Other files are for testing or documentation purposes and should not be used as an API. The following are the currently implemented endpoints:
  <ul>
  <li>/getSetUnion</li>
  <li>/getSetIntersect</li>
  <li>/getAsymDiff</li>
  </ul>


  <h2>Endpoints in Detail</h2>

  <h4>/getSetUnion(m,n) </h4>
  <p> @param   n     The number of sets to be joined. Default value = 2 </p>
  <p> @param   m     The number of elements in each set. Default value = 5 </p>
  <p> @returns       A Set Union multiple choice problem in the form of a JSON Object. The returned JSON object adheres to the following structure: </p>

  `{source: ["Question Text", sets that need to be joined],` <br>
    `answer: [The correct results of the union of the source sets],`<br>
    `wrongs: [[distractor answer 1],` <br>
             `[distractor answer 2],` <br>
             `[distractor answer 3]]` <br>
  `}`

  
  Consider the following example output:
  
  `{ source: [["Select the correct union of the following sets"],[14,15,2,4,12],[4,1,18,7,10]],`<br>
     `answer: [1,2,4,7,10,12,14,15,18],` <br>
     `wrongs: [[1,2,4,4,7,10,12,14,15,18],`<br>
              `[1,2,4,7,9,10,10,12,15,17],` <br>
              `[1,5,6,9,10,12,15,17,18,20]]`<br>
  `}`
  
  <h4>/getSetIntersect(m,n) </h4>
  <p> @param   n     The number of sets to be joined. Default value = 2 </p>
  <p> @param   m     The number of elements in each set. Default value = 5 </p>
  <p> @returns       A Set Intersection multiple choice problem in the form of a JSON Object. The returned JSON object adheres to the following structure: </p>
  
  `{source: ["Question Text", sets to be considered],` <br>
    `answer: [The correct results of the intersection of the source sets],`<br>
    `wrongs: [[distractor answer 1],` <br>
             `[distractor answer 2],` <br>
             `[distractor answer 3]]` <br>
  `}`
        
  
  Consider the following example output:
  
  `{ source: [["Select the correct intersection of the following sets"],[6,8,4,5,7],[9,4,6,2,8]],`<br>
     `answer: [4,6,8],` <br>
     `wrongs: [[6,8,4,5,7,9,4,6,2,8],`<br>
              `[4,4,6,6,8,8],` <br>
              `[5,7,9,2]]`<br>
  `}`

<h4>/getAsymDiff(m,n) </h4>
  <p> @param   n     The number of sets to be considered. Default value = 2 </p>
  <p> @param   m     The number of elements in each set. Default value = 5 </p>
  <p> @returns       A Set Difference multiple choice problem in the form of a JSON Object. The returned JSON object adheres to the following structure: </p>
  
  `{source: ["Question Text", sets to be considered],` <br>
    `answer: [The correct results of the difference of the source sets A-B],`<br>
    `wrongs: [[distractor answer 1],` <br>
             `[distractor answer 2],` <br>
             `[distractor answer 3]]` <br>
  `}`
        
  
  Consider the following example output:
  
  `{ {\"source\":[[\"Select the correct set difference A-B where the first set is A and the second is B\"],[9,6,5,1,3],[8,7,9,4,6]],`<br>
     `answer: [5,1,3],` <br>
     `wrongs: [[1,3,4,5,7,8],`<br>
              `[4,7,8],` <br>
              `[9,6]]`<br>
  `}`
  
  <h4>/getSetComplement(numSets, setSize, dType) </h4>
  <p> @param   numSets     The number of sets to be considered. Default value = 2 </p>
  <p> @param   setSize     The number of elements in each set. Default value = 5 </p>
  <p> @param   dType       The datatype which will populate the sets </p>
  <p> @returns       A Set Complement multiple choice problem in the form of a JSON Object. The returned JSON object adheres to the following structure: </p>
  
  `{source: ["Question Text", sets to be considered],` <br>
    `answer: [The correct results of the difference between the source set A and the universal set B.],`<br>
    `wrongs: [[distractor answer 1],` <br>
             `[distractor answer 2],` <br>
             `[distractor answer 3]]` <br>
  `}`
        
  
  Consider the following example output:
  
  `{ {\"source\":[[\"Let A be a set and B be the universal set. What is the complement of set A?\"],[9,6,5,1,3],[9,6,5,1,3,8,7,9,4,]],`<br>
     `answer: [8,7,9,4],` <br>
     `wrongs: [[8,3,9,4],`<br>
              `[9,7,9,4],` <br>
              `[9,6,5,1,3]]`<br>
  `}`
  
  <h4>/getSetEquality(numSets, setSize, dType) </h4>
  <p> @param   numSets     The number of sets to be considered. Fixed to 2 as function only supports two sets. </p>
  <p> @param   setSize     The number of elements in each set. Default value = 9 </p>
  <p> @param   dType       The datatype which will populate the sets </p>
  <p> @returns       A Set Equality multiple choice problem in the form of a JSON Object. The returned JSON object adheres to the following structure: </p>
  
  `{source: ["Question Text", sets to be considered],` <br>
    `answer: [The correct answer will be one of two strings "Equal" or "Not Equal" depending on the two sets.],`<br>
    `wrongs: [[distractor answer 1],` <br>
  `}`
        
  
  Consider the following example output:
  
  `{ {\"source\":[[\"Let A and B be two sets. Are A and B equal?\"],[9,6,5,1,3],[9,6,5,1,3]],`<br>
     `answer: [Equal]` <br>
     `wrongs: [Not Equal],`<br>
  `}`
  
  <h4>/getSetCardinality(numSets, setSize, dType) </h4>
  <p> @param   numSets     The number of sets to be considered. Default value = 1 </p>
  <p> @param   setSize     The number of elements in each set. Will choose a random number between 1 and 9. </p>
  <p> @param   dType       The datatype which will populate the sets </p>
  <p> @returns       A Set Cardinality multiple choice problem in the form of a JSON Object. The returned JSON object adheres to the following structure: </p>
  
  `{source: ["Question Text", set to be considered],` <br>
    `answer: [The correct answer is the number which corresponds to the cardinality of the generated set.],`<br>
    `wrongs: [[distractor answer 1],` <br>
             `[distractor answer 2],` <br>
             `[distractor answer 3]]` <br>
  `}`
        
  
  Consider the following example output:
  
  `{ {\"source\":[[\"Let A be a set. What is the cardinality of set A?\"],[9,6,5,1,3,8,7,9]],`<br>
     `answer: [8],` <br>
     `wrongs: [7],`<br>
              `[9],` <br>
              `[13]]`<br>
  `}`