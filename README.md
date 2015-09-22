Divvy
=====

Given a list of expenses, optionally preceded by a list of
participants, divvy figures out how much each person owes (or is owed)
and produces a list of payment instructions that minimizes the total
number of payments.

Individual expenses may be marked as only applying to a subset of the
group.  For example, if the overall shared expense is a vacation but a
particular meal was shared by only half the group, only those members
end up owing money for the meal.

Finally divvy can also handle creditors (someone who paid money) but
did not actually participate. Such individuals are paid back in full
what they spent.

Input Format
============

Input is in the form of a simple domain specific language. Names are
always alphabetic strings with no other characters (no punctuation, no
numbers). Lists of participants are comma separated. Use of the word
and before the last participant is optional. The Oxford comma is
optional but you should use it anyway.

The first line of the input is an optional list of participant
names. Participants always share group-wide expenses:

    participants: alice, bob, carol, and dave

The participant list is only necessary if there is a creditor that did
not participate. For example, if someone paid for a ticket that was
then used by another person.

Each subsequent line contains an expense. Group-wide expenses look
like this:

    alice spent $52.82 on beer

If only a subset of the participants shared an expense, they can be
called out:

    bob spent $25 for bob and carol on wine

Non-participant creditors have their expenses listed as usual:

    eve spent $100 on tickets

Output
======

> sbt "run example2"

    [info] Running us.zuercher.divvy.Main example2
    david pays eve 38.20
    bob pays eve 25.71
    carol pays eve 36.09
    carol pays alice 14.61
    [success] Total time: 1 s, completed Sep 22, 2015 9:29:37 AM
