# FreeCodeCamp JavaScript Certification

### Palindrome Checker

Return true if the given string is a palindrome. Otherwise, return false.

A palindrome is a word or sentence that's spelled the same way both forward and backward, ignoring punctuation, case, and spacing.

Note: You'll need to remove all non-alphanumeric characters (punctuation, spaces and symbols) and turn everything into the same case (lower or upper case) in order to check for palindromes.

We'll pass strings with varying formats, such as racecar, RaceCar, and race CAR among others.

We'll also pass strings with special symbols, such as 2A3*3a2, 2A3 3a2, and 2_A3*3#A2.

### Roman Numeral Converter

Convert the given number into a roman numeral.<br>

<table>
<tr>
    <td>Roman numerals</td>	
    <td>Arabic numerals</td>
</tr>
<tr>
    <td>M</td>	
    <td>1000</td>
</tr>
<tr>
    <td>CM</td>	
    <td>900</td>
</tr>
<tr>
    <td>D</td>	
    <td>500</td>
</tr>
<tr>
    <td>CD</td>	
    <td>400</td>
</tr>
<tr>
    <td>C</td>	
    <td>100</td>
</tr>
<tr>
    <td>XC</td>	
    <td>90</td>
</tr>
<tr>
    <td>L</td>	
    <td>50</td>
</tr>
<tr>
    <td>XL</td>	
    <td>40</td>
</tr>
<tr>
    <td>X</td>	
    <td>10</td>
</tr>
<tr>
    <td>IX</td>	
    <td>9</td>
</tr>
<tr>
    <td>V</td>	
    <td>5</td>
</tr>
<tr>
    <td>IV</td>	
    <td>4</td>
</tr>
<tr>
    <td>I</td>	
    <td>1</td>
</tr>
</table>

All roman numerals answers should be provided in upper-case.

### Caesar Cypher

One of the simplest and most widely known ciphers is a Caesar cipher, also known as a shift cipher. In a shift cipher the meanings of the letters are shifted by some set amount.

A common modern use is the <a href="https://www.freecodecamp.org/news/how-to-code-the-caesar-cipher-an-introduction-to-basic-encryption-3bf77b4e19f7/">ROT13</a> cipher, where the values of the letters are shifted by 13 places. Thus A ↔ N, B ↔ O and so on.

Write a function which takes a ROT13 encoded string as input and returns a decoded string.

All letters will be uppercase. Do not transform any non-alphabetic character (i.e. spaces, punctuation), but do pass them on.