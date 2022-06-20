// Return true if the given string is a palindrome. Otherwise, return false.

const rot13 = str => {
    // a look up table for alphabet
    const alphabet = ["A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z"]
    // create a variable to store rotations
    let counter = ''
    // for each element of array, find corresponding place in alphabet
    for (let i=0; i<str.length;i++) {
        // if element not a letter, add to counter
        if (!alphabet.includes(str[i])) {
            counter += str[i]
        } else {
            // find index within alphabet 
            let index = alphabet.findIndex((c) => c===str[i]) 
            // if index is greater than or equal to 13, subtract 13
            if (index >= 13 ) {
                counter += alphabet[index-13]   
            } else {
                // otherwise, add 13
                counter += alphabet[index+13]
            }
        }
    }    
    return counter;
  }