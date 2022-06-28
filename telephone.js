// Telephone Number Validator
// Return true if the passed string looks like a valid US phone number.

const telephoneCheck = str => {
    let regex = /^\d{3}-\d{3}-\d{4}$|^\(\d{3}\)-\d{3}-\d{4}$|^\(\d{3}\) \d{3}-\d{4}$|^\d{3} \d{3} \d{4}$|^\d{10}$|^1 \d{3} \d{3} \d{4}$|^1 \d{3}-\d{3}-\d{4}$|^1 \(\d{3}\) \d{3}-\d{4}$|^\(\d{3}\)\d{3}-\d{4}$|^1\(\d{3}\)\d{3}-\d{4}/g
    return regex.test(str);
  }



 