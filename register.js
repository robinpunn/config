const checkCashRegister = (price, cash, cid) => {
    // calculate amount of change due, multiply by 100 to avoid decimals
    let change = cash*100 - price*100
    
    // use for loop to get a total of all the cash in register
    let counter = 0
    for (let cash of cid) {
      counter += cash[1]*100
    }
    
    // use if statements to run through all scenarios
    if (change > counter) {
      // if the amount of change is greater than total in register  
      return {status: "INSUFFICIENT_FUNDS", change: []}
    } else if (change === counter) {
      // if the amount of change is equal to the register  
      return {status: "CLOSED", change: cid}
    } else {
          // create an empty array to hold change values  
          let final = []
          // reverse cid 
          cid = cid.reverse()
          // keep track of each type of currency and its value
          let bank = { 
              "ONE HUNDRED":10000,
              "TWENTY":2000,
              "TEN": 1000,
              "FIVE": 500,
              "ONE": 100,
              "QUARTER":25,
              "DIME": 10,
              "NICKEL": 5,
              "PENNY": 1
          }
          // use for loop to cycle through elements of cid  
          for (let cash of cid) {
              // give is an array storing the type of currency, and amount given
              let give = [cash[0],0]
              // multiple cid values by 100
              cash[1]*=100
              
              while(change >= bank[cash[0]] && cash[1]) {    
                  change -= bank[cash[0]]
                  cash[1] -= bank[cash[0]]
                  give[1] += bank[cash[0]]/100
              }
              if (give[1] > 0) {
                  final.push(give)
              }
          }
          if (change > 0) {
              return {status: "INSUFFICIENT_FUNDS", change: []}
          }
          return {status: "OPEN", change: final}  
      }
  }
  
  checkCashRegister(19.5, 20, [["PENNY", 1.01], ["NICKEL", 2.05], ["DIME", 3.1], ["QUARTER", 4.25], ["ONE", 90], ["FIVE", 55], ["TEN", 20], ["TWENTY", 60], ["ONE HUNDRED", 100]]);