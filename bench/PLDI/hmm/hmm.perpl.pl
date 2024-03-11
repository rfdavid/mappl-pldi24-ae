#!/usr/bin/perl
#
# Usage: pcfg-gen-perpl $length

use strict;
my $length = $ARGV[0];

print<<'END';
-- f"{torch.exp(dist.Normal(1,1).log_prob(torch.tensor([1]))).item():.30f}"
-- Out[54]: '0.398942291736602783203125000000'
-- f"{torch.exp(dist.Normal(1,1).log_prob(torch.tensor([0]))).item():.30f}"
-- Out[55]: '0.241970732808113098144531250000'
-- f"{torch.exp(dist.Normal(0,1).log_prob(torch.tensor([1]))).item():.30f}"
-- Out[56]: '0.241970732808113098144531250000'
-- f"{torch.exp(dist.Normal(0,1).log_prob(torch.tensor([0]))).item():.30f}"
-- Out[57]: '0.398942291736602783203125000000'

-- f"{torch.exp(dist.Normal(1,0.3).log_prob(torch.tensor([1]))).item():.30f}"
-- Out[58]: '1.329807639122009277343750000000'
-- f"{torch.exp(dist.Normal(1,0.3).log_prob(torch.tensor([0]))).item():.30f}"
-- Out[59]: '0.005140930414199829101562500000'
-- f"{torch.exp(dist.Normal(0,0.3).log_prob(torch.tensor([1]))).item():.30f}"
-- Out[60]: '0.005140930414199829101562500000'
-- f"{torch.exp(dist.Normal(0,0.3).log_prob(torch.tensor([0]))).item():.30f}"
-- Out[61]: '1.329807639122009277343750000000'

data String = Nil | Cons Bool String;
define observe: Bool -> Bool -> () = \cur. \obs.
    if cur then
        if obs then
            factor 1.329807639122009277343750000000 in
            ()
        else
            factor 0.005140930414199829101562500000 in
            ()
    else
        if obs then
            factor 0.005140930414199829101562500000 in
            ()
        else
            factor 1.329807639122009277343750000000 in
            ()
;

define transite: Bool -> Bool = \cur.
    if cur then
        amb (factor 0.7 in True) (factor 0.3 in False)
    else
        amb (factor 0.3 in True) (factor 0.7 in False)
;

define hmma: Bool -> String -> () = \cur. \obs.
    case obs of 
    | Nil -> ()
    | Cons x xs -> 
        let _ = (observe cur x) in
        let nxt = (transite cur) in
        hmma nxt xs
;

END

print "define obs =", " (Cons True" x $length, " Nil", ")" x $length, ";\n";

print<<'END';

hmma True obs
END


