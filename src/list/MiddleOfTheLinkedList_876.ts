// T(n) = O(n)
// S(n) = O(1)

import { ListNode } from "./ListNode";

function middleOfTheLinkedList(head: ListNode) {
    function helper(slow: ListNode, fast: ListNode | null) {
        if (fast === null || fast.next === null) {
            return slow;
        }

        const slowNext = slow.next;
        const fastNext = fast.next.next;
        if (slowNext === null) {
            return slow;
        }
        if (fastNext === null) {
            return slowNext;
        }
        return helper(slowNext, fastNext);
    }

    const slow = head;
    if (slow === null) {
        return new ListNode(0);
    }
    const fast = head.next;
    if (fast === null) {
        return slow;
    }
    return helper(slow, fast);
}

let l1 = new ListNode(1);
let l2 = new ListNode(2);
let l3 = new ListNode(3);
let l4 = new ListNode(4);
let l5 = new ListNode(5);
let l6 = new ListNode(6);
l1.next = l2;
l2.next = l3;
l3.next = l4;
l4.next = l5;
l5.next = l6;
let r1 = middleOfTheLinkedList(l1);
console.log("r1: ", r1);
