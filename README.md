# Set-Operations-api
Set Operations API

<h2>API functionallity</h2>

Currently implemented API functions are found within the `set-relation.R` file. Other files are for testing or documentation purposes and should not be used as an API. The following are the currently implemented endpoints:
  <ul>
  <li>/getSetUnion</li>
  <li>/getSetIntersect</li>
  <li>/getAsymDiff</li>
  <li>/getSetComplement</li>
  <li>/getSetEquality</li>
  <li>/getSetCardinality</li>
  <li>/getSymmDiff</li>
  <li>/getSetPartitions</li>
  </ul>


  <h2>Endpoints in Detail</h2>

  <h4>/getSetUnion(qType, qDifficulty, dataType) </h4> 

  Topic:  `setUnion`

  <h5>Parameters</h5>  
  
```
  @param    qType           Question Type ('1': Multiple Choice)
  
  @param    qDifficulty     Question Difficutly (1:easiest - 5:hardest)
  
  @param    dataType        Data type (1: Ints, 2: Real, 3: Complex, 
                                       4: Char, 5: String, 6: Mixed) 
```

The return is as follows:

```
  {
  "topic": [
    "setUnion"
  ],
  "type": [
    "1"
  ],
  "format": [
    "1"
  ],
  "difficulty": [
    "1"
  ],
  "question": {
    "content": [
      [
        "Let A and B be two sets. What is \\$A\\cup B\\$?"
      ],
      [
        "\\$A=\\$ \\$\\{ 13, 3, 4, 8, 12  \\}\\$"
      ],
      [
        "\\$B=\\$ \\$\\{ 17, 8, 5, 15, 13  \\}\\$"
      ]
    ],
    "correct": [
      "\\$\\{ 13, 3, 4, 8, 12, 17, 5, 15  \\}\\$"
    ],
    "distractors": [
      [
        "\\$\\{ 13, 3, 4, 12, 17, 5, 15  \\}\\$"
      ],
      [
        "\\$\\{ 13, 3, 4, 8, 12, 17, 5, 15, 5  \\}\\$"
      ],
      [
        "\\$\\{ 13, 3, 8, 12, 17, 5, 15  \\}\\$"
      ]
    ]
  }
}

```


  <h4>/getSetIntersect(qType, qDifficulty, dataType) </h4>
 
  Topic:  `setIntersect`

  <h5>Parameters</h5>  
  
```

  @param    qType           Question Type ('1': Multiple Choice)
  
  @param    qDifficulty     Question Difficutly (1:easiest - 5:hardest)
  
  @param    dataType        Data type (1: Ints, 2: Real, 3: Complex, 
                                       4: Char, 5: String, 6: Mixed) 
```

<h4>/getAsymDiff(qType, qDifficulty, dataType) </h4>
  
  Topic:  `asymmetricDifference`

  <h5>Parameters</h5>  
  
```
  @param    qType           Question Type ('1': Multiple Choice)
  
  @param    qDifficulty     Question Difficutly (1:easiest - 5:hardest)
  
  @param    dataType        Data type (1: Ints, 2: Real, 3: Complex, 
                                       4: Char, 5: String, 6: Mixed) 
```
  
  <h4>/getSetComplement(qType, qDifficulty, dataType) </h4>
  
  Topic:  `setComplement`

  <h5>Parameters</h5>  
  
```

  @param    qType           Question Type ('1': Multiple Choice)
  
  @param    qDifficulty     Question Difficutly (1:easiest - 5:hardest)
  
  @param    dataType        Data type (1: Ints, 2: Real, 3: Complex, 
                                       4: Char, 5: String, 6: Mixed) 
                                       
```
  
  <h4>/getSetEquality(qType, qDifficulty, dataType) </h4>
  
  Topic:  `setEquality`

  <h5>Parameters</h5>  
  
```

  @param    qType           Question Type ('1': Multiple Choice)
  
  @param    qDifficulty     Question Difficutly (1:easiest - 5:hardest)
  
  @param    dataType        Data type (1: Ints, 2: Real, 3: Complex, 
                                       4: Char, 5: String, 6: Mixed) 
                                       
```
  
  <h4>/getSetCardinality(qType, qDifficulty, dataType) </h4>
  
  Topic:  `setCardinality`

  <h5>Parameters</h5>  
  
```

  @param    qType           Question Type ('1': Multiple Choice)
  
  @param    qDifficulty     Question Difficutly (1:easiest - 5:hardest)
  
  @param    dataType        Data type (1: Ints, 2: Real, 3: Complex, 
                                       4: Char, 5: String, 6: Mixed) 
```

 <h4>/getSymmDiff(qType, qDifficulty, dataType) </h4>
  
  Topic:  `symmetricDifference`

  <h5>Parameters</h5>  
  
```

  @param    qType           Question Type ('1': Multiple Choice)
  
  @param    qDifficulty     Question Difficutly (1:easiest - 5:hardest)
  
  @param    dataType        Data type (1: Ints, 2: Real, 3: Complex, 
                                       4: Char, 5: String, 6: Mixed) 
```

<h4>/getSetPartitions(qType, qDifficulty, dataType) </h4>
  
  Topic:  `setPartitions`

  <h5>Parameters</h5>  
  
```

  @param    qType           Question Type ('1': Multiple Choice)
  
  @param    qDifficulty     Question Difficutly (1:easiest - 5:hardest)
  
  @param    dataType        Data type (1: Ints, 2: Real, 3: Complex, 
                                       4: Char, 5: String, 6: Mixed) 
                                       
```

 <h4>/setExpressions(qType, qDifficulty) </h4>
  
  Topic:  `setExpressions`

  <h5>Parameters</h5>  
  
```

  @param    qType           Question Type ('1': Multiple Choice)
  
  @param    qDifficulty     Question Difficutly (1:easiest - 5:hardest)
  
 
```

<h4>/powerSetQ(qType, qDifficulty, dataType) </h4>
  
  Topic:  `powerSet`

  <h5>Parameters</h5>  
  
```

  @param    qType           Question Type ('1': Multiple Choice)
  
  @param    qDifficulty     Question Difficutly (1:easiest - 5:hardest)
  
  @param    dataType        Data type (1: Ints, 2: Real, 3: Complex, 
                                       4: Char, 5: String, 6: Mixed) 
                                       
```

<h4>/cartesianProduct(qType, qDifficulty, dataType) </h4>
  
  Topic:  `cartesianProduct`

  <h5>Parameters</h5>  
  
```

  @param    qType           Question Type ('1': Multiple Choice)
  
  @param    qDifficulty     Question Difficutly (1:easiest - 5:hardest)
  
  @param    dataType        Data type (1: Ints, 2: Real, 3: Complex, 
                                       4: Char, 5: String, 6: Mixed) 
                                       
```
