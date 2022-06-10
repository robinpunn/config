const convertToRoman = num => {
    
    // look up table for roman numberals
    const roman = {
      'M':1000,
      'CM':900,
      'D': 500,
      'CD': 400,
      'C': 100,
      'XC': 90,
      'L': 50,
      'XL':40,
      'X':10,
      'IX': 9,
      'V': 5,
      'IV': 4,
      'I': 1
    }
    
    // variable to store roman numerals
    let counter = ''
    
    // loop through table
    for (let numeral in roman) {
        // while current number is <= input, subtranct current from input, 
        // add symbol to counter
        while (roman[numeral] <= num) {
            num -= roman[numeral]
            counter += numeral
        }
    }
   
   // return counter
   return counter;
  }

