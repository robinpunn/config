const palindrome = str => {
    //create variable to hold modified str
    let clean = str
        // make all elements lowercase
        .toLowerCase()
        // use match to create an array including only alphanumeric characters
        .match(/[a-z0-9]/g)
    // convert back to string
    // return a boolean comparing reversed     
    return clean.join('') === clean.reverse().join('')    
}