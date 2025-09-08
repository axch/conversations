---
title: Do Carbon Offsets Really Work? 
date: 2021/10/27
author: Alexey Radul
published: true
---

A friend recently pointed me to [an
article](https://www.cesifo.org/en/publikationen/2021/working-paper/do-carbon-offsets-offset-carbon)
arguing that the Clean Development Mechanism may be counter-productive
if it mis-estimates how additional its supported projects are, and
ends up allowing more emissions elsewhere than it actually reduces.
Does this mean offsets are useless?  READMORE  As an individual, does buying and
retiring offsets still hold water as a climate charity?  Let's work
through a contrived example and see what happens.

Context
-------

For context, the CDM is the world's largest carbon-offset market.  If
I understand correctly, the idea is this: There are relatively
easy-to-regulate carbon emitters, like fossil-fuel power plants.  They
can be subjected to a cap-and-trade scheme, where the regulatory
authority demands that each one emit no more mega-tons of
CO2-equivalent than its allowance.  To take advantage of markets to
maximize efficiency, the regulatory authority allocates a single total
allowance (cap) for the whole sector, and auctions the individual
allowances under the cap to whoever is willing to pay for them.  Thus,
power plants that can reduce emissions the most cheaply can do that,
and those that can't buy more of the allowances, achieving the
required emissions limit at the lowest cost.  As an individual, you
can further push the world to reduce carbon emissions by buying some
of the allowances and retiring them without emitting anything,
effectively paying to lower the cap.[^political-economy]

[^political-economy]: Assuming, of course, that you trust the caps to
be set wisely and exogenously, based on scientific climate mitigation
goals rather than on observed prices for the allowances.  In the
latter case, burning allowances just causes the authority to slow down
the pace at which the cap is lowered, defeating the purpose.  That
problem is out of scope for this essay, though.

So far so good.  Where do offsets and the CDM come in?  Here the idea
is that reducing emissions from even the easiest-to-clean power plants
may not be the most effective use of money.  Other reduction projects
are harder to measure and regulate, though, so the regulatory
authority can't do it reliably.  Enter the CDM.  They are supposed to
do a bunch of due diligence evaluating various emissions reduction
projects according to their criteria, notably including
"additionality".  If a project meets the criteria, the CDM funds the
project, and creates an offset, which is a legal certificate saying
"because we paid them, this project reduced emissions by X mega-tons
that otherwise wouldn't have happened, and it's all for real."  Then
the regulatory authority in our cap-and-trade power market accepts
this certificate as equivalent to a tradable X-mega-ton emissions
allowance.  If the CDM's projects are cheap enough, the regulated
emitters will buy these certificates, thus funding the CDM's projects
instead of performing their own (presumably more expensive) emissions
mitigations, ultimately achieving the required emissions reductions
even more cost-effectively.

Problem
-------

Sounds great!  But, as Calel et. al. point out, what if the CDM is
wrong, and its projects aren't actually as additional as it thought?
Then it's handing out money to projects that would have happened
anyway, and letting regulated emitters off the hook by selling them
offsets that don't actually correspond to real reductions.  How bad is
this?

Let's work through some contrived numbers.  Suppose we are working
with a regulated power market that, in the absence of the CDM, would
clear at $26/T for 30 GT of emissions allowances.  Suppose the CDM
funds $10B worth of emissions reductions projects that the CDM thinks
reduce emissions by 1 GT.  Following its rules, the CDM proceeds to
create 1 GT worth of offset certificates and (I assume) offer them
on the regulated market at $10/T.

The referenced paper is a study of wind farms in India, comparing
those the CDM funded with similar ones it did not.  It comes to the
conclusion that at least 52% of the farms would have been built
anyway, so let's go with that: we assume in our example that the CDM
is wrong by 2x, and in fact its $10B only produced 0.5 GT of really
additional emissions reductions.  What happens now?

### Scenario A

As a baseline, what if the CDM just doesn't issue those
offsets in the first place?  Then the regulated market is irrelevant,
and the CDM is charity that thinks it's getting reductions at a rate
of $10/T, but it's actually paying $20/T.  Given the severity of the
climate disaster, that's probably still worth it.

### Scenario B

CDM makes its offsets and offers them on the market.
Let's assume that the increased supply means the market now closes at
$25/T for 31 GT of allowances instead of $26/T for 30 GT.  Then the
CDM:

1.  made a profit of $15B;
2.  increased total emissions by 0.5 GT;
3.  reduced in-market costs faced by emitters by (26 * 30 - 25 * 31) = $5B;
4.  reduced out-of-market compliance costs for
    emitters by some amount between $25B and $26B (because at those market
    clearing prices, that must be what that 1 GT of reductions would have
    cost to implement by the regulated emitters); and
5.  reduced the revenue of the regulatory authority by $30B.

So in this scenario, the CDM is itself a polluter with revenue $50/T
and profit $30/T; which cheated the regulatory authority by increasing
the effective cap in the sector by 0.5 GT.  The authority can fix it
post-hoc by lowering the cap by 0.5 GT, but the CDM's existence is
still post-hoc "justified" if we buy the logic of cap-and-trade
markets, as it's generating "more value" from the limited pool of
allowed emissions than the marginal other market participant.[^follow-the-money]

[^follow-the-money]: What about the money?  Who is really paying for
what here?  Well, that depends on what our modeled entities do with
their money.  One (charitable!) assumption is that the regulatory
authority and the CDM, being public institutions, return all their
profits to the public as a dividend or as reduced taxes; and the
emitters are forced by competition to pass their savings along to the
public as well, in the form of reduced electricity prices.  Then the
public gains $15-16B in aggregate, which came from giving $25-26B less
in work to the power-plant-emissions-reduction industry and instead
giving $10B more to the CDM-funded projects.  It also cost 0.5 GT more
emissions, coming out at $30-32/T.  The gain of $15-16B is
uneven, though: $25-26B in savings are distributed in proportion to
consumption of electricity (including indirectly through the cost of
energy-intensive but competitively priced goods), while $15B of costs
are imposed in proportion to payment of taxes.

Even in this setting, buying offsets remains a reasonable climate
charity.  The existence of the offset mechanism has silently increased
the real emissions cap, but as long as that cap remains binding,
buying allowances still forces marginal reductions (in this case at a
cost of $25/T).

### Scenario C

CDM catches itself, and creates 0.5 GT of offsets instead
of 1 GT, which it offers at $20/T instead of $10/T.[^offer-price]  Supply in the
emissions market again increases, but not as much as in Scenario B, so
maybe now the market clears at $25.5/T.  Then the CDM:

1.  made a profit of $2.75B;
2.  was emissions-neutral;
3.  reduced in-market costs faced by emitters by (26 * 30 - 25.5 * 30.5) = $2.25B;
4.  reduced out-of-market compliance costs for emitters by some amount
    between $12.75B and $13B; and
5.  reduced the revenue of the regulatory authority by $15B.

So now the CDM is doing what it was designed to do: achieving the
emissions targets set by the regulatory authority at lower overall cost
(and therefore probably higher political feasibility).

[^offer-price]: Why do I bother with the CDM's offer price, if the
market clears above it anyway?  Because the gap between offer and
clearing is the CDM's signal to fund more or fewer projects.  If we
assume the CDM has the organizational capacity to find and fund enough
projects to equalize its cost to the clearing price in the regulated
market, then pricing offsets higher to account for diligence errors
will reduce the number of projects the CDM can fund.  This is
presumably good, because it lowers the magnitude of the mistake the
CDM is making under these assumptions.

Conclusion
----------

Of course, all the above has lots of assumptions and moving parts, but
a few themes seem durable to me (assuming that one trusts
regulated cap-and-trade markets to work as advertised, and to be big
enough to absorb the offsets):

1.  Carbon offset markets can indeed backfire, in the sense of
    accidentally increasing effective emissions caps (but not by more
    than the volume of the new offsets).

2.  If the realized additionality of funded projects can be measured
    after the fact, the backfire can be fixed by lowering caps to
    compensate.

3.  Charitable offset purchases still work, but they only work as
    advertised because they reduce the effective cap in the regulated
    market into which those offsets would otherwise be sold.

4.  If the offsets are not tied to a trustworthy regulated market,
    we're in Scenario A.  A buyer can still obtain 1 T of reductions
    by buying offsets, but it becomes necessary to pay $20 for 2 T
    worth of offset certificates to do so.

I like point 2 above.  Everything is easier to measure after the fact
than before, including how additional an emissions reduction project
really was.  I wonder whether some energetic economist can work out a
good policy for cap-and-trade market authorities to follow when
accepting offsets, along the lines of "We'll honor your offset
certificates today, but we'll measure how good they were later and
compensate with automatic additional cap reductions."  Maybe with some
mechanism for pushing money around too, so that not-really-additional
offset projects don't become too good of a swindle.

Notes
-----
