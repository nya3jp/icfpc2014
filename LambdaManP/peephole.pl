# use Clone qw(clone);

sub clone($){
	my @a = @{$_[0]};
	my @b;
	foreach $a (@a){ push(@b,$a); }
	return \@b;
}

$top = 10;
@stack_init = (0...$top);

@stack = @stack_init;
@insts_type = ();
@insts = ();
@insts_stack = ();
while($line = <STDIN>){
	$line =~ s/[\r\n]//g;
	$line =~ s/\s+$//g;
	$line =~ s/^\s+//g;
	if( $line =~ /^(.*):$/ ){
		$label = $1;
		@stack = @stack_init;
		push(@insts_type, "label");
		push(@insts, "$line");
		push(@insts_stack_before, clone(\@stack));
		push(@insts_stack, clone(\@stack));
	}
	else{
		push(@insts_stack_before, clone(\@stack));

		@arg = split /\s+/, $line;
		if( $arg[0] eq "LDC" ){ push(@stack,"CONST=$arg[1]"); }
		elsif( $arg[0] =~ /^LD|LDF$/ ){ push(@stack,"?"); }
		elsif( $arg[0] =~ /^ADD|SUB|MUL|DIV|CEQ|CGT|CGTE|CONS$/ ){ pop(@stack);pop(@stack);push(@stack,"?"); }
		elsif( $arg[0] =~ /^ATOM|CAR|CDR$/ ){ pop(@stack);push(@stack,"?"); }
		else{ @stack = @stack_init; print STDERR  "ERROR: $arg[0]\n"; }

		push(@insts_type, $arg[0]);
		push(@insts, $line);
		push(@insts_stack, clone(\@stack));
	}
}

for $i (0...$#insts){
	if( $insts_type[$i] eq 'label' ){
		for( $j = $i+1; $j <= $#insts; $j ++ ){
			@arg = split /\s+/, $insts[$j];
			if( $arg[0] =~ /^TRAP|TAP|TSEL|RTN|JOIN$/ ){ last; }
		}
		if( $j <= $#insts ){
			# [$i+1,$j], inclusive
			$len = $j - $i;
			if( $len < 100 ){
				$goto_i{$insts[$i]} = $i+1;
				$goto_j{$insts[$i]} = $j;
			}
		}
	}
	if( $insts_type[$i] eq 'label' && $insts_type[$i+1] eq 'TSEL' ){
		@arg = split /\s+/, $insts[$i+1];
		$goto_then{$insts[$i]} = $arg[1];
		$goto_else{$insts[$i]} = $arg[2];
		$goto_arg{$insts[$i]}  = 1;
		print STDERR  "TSEL $arg[1] $arg[2]\n";
	}
	if( $insts_type[$i] eq 'label' && $insts_type[$i+1] eq 'LDC' && $insts_type[$i+2] eq 'TSEL' ){
		@ldc = split /\s+/, $insts[$i+1];
		@arg = split /\s+/, $insts[$i+2];
		print STDERR  "LDC $ldc[1] / TSEL $arg[1] $arg[2]\n";
		if( $ldc[1] eq '0' ){
			$goto_then{$insts[$i]} = $arg[2];
			$goto_else{$insts[$i]} = $arg[2];
			$goto_arg{$insts[$i]}  = 0;
		}
		else{
			$goto_then{$insts[$i]} = $arg[1];
			$goto_else{$insts[$i]} = $arg[1];
			$goto_arg{$insts[$i]}  = 0;
		}
	}
}

foreach $l (keys %goto_arg){
	print STDERR  "$l: $goto_arg{$l} / $goto_then{$l} / $goto_else{$l}\n";
}

for $i (0...$#insts){
	print STDERR  "$i: $insts_type[$i] <$insts[$i]>";
	print STDERR  "BEFORE=[";
	for $s (@{$insts_stack_before[$i]}){
		print STDERR  "$s ";
	}
	print STDERR  "] ";
	print STDERR  "AFTER=[";
	for $s (@{$insts_stack[$i]}){
		print STDERR  "$s ";
	}
	print STDERR  "]\n";
}

print STDERR "=================================\n";

for $i (0...$#insts){
	$output = 0;
	if( $insts_type[$i] eq 'LDC' && $insts_type[$i+1] eq 'TSEL' ){
		@ldc = split /\s+/, $insts[$i];
		@arg = split /\s+/, $insts[$i+1];
		if( $ldc[1] eq '0' ){
			$target = $arg[2];
		}
		else{
			$target = $arg[1];
		}
		$target .= ":";
		print STDERR "REPLACE? LDC $ldc[1] / TSEL $arg[1] $arg[2] / TARGET = $target\n";
		if( $goto_arg{$target} eq '0' ){
			print "LDC 0\n";
			print "TSEL $goto_then{$target} $goto_then{$target}\n";
			$output = 1;
			$i += 1;
		}
		elsif( $goto_arg{$target} eq '1' ){
			print "TSEL $goto_then{$target} $goto_else{$target}\n";
			$output = 1;
			$i += 1;
		}
		elsif( $goto_i{$target} ne '' ){
			for( $j = $goto_i{$target}; $j <= $goto_j{$target}; $j ++ ){
				if( $insts_type[$j] ne 'label' ){
					print "$insts[$j]\n";
				}
			}
			$output = 1;
			$i += 1;
		}
	}

	elsif( $insts_type[$i] eq 'LDC' && $insts_type[$i+1] eq 'label' && $insts_type[$i+2] eq 'TSEL' ){
		@ldc = split /\s+/, $insts[$i];
		@arg = split /\s+/, $insts[$i+2];
		if( $ldc[1] eq '0' ){
			$target = $arg[2];
		}
		else{
			$target = $arg[1];
		}
		$target .= ":";
		print STDERR "REPLACE? LDC $ldc[1] / TSEL $arg[1] $arg[2] / TARGET = $target\n";
		if( $goto_arg{$target} eq '0' ){
			print "LDC 0\n";
			print "TSEL $goto_then{$target} $goto_then{$target}\n";
			$output = 1;
		}
		elsif( $goto_arg{$target} eq '1' ){
			print "TSEL $goto_then{$target} $goto_else{$target}\n";
			$output = 1;
		}
		elsif( $goto_i{$target} ne '' ){
			for( $j = $goto_i{$target}; $j <= $goto_j{$target}; $j ++ ){
				if( $insts_type[$j] ne 'label' ){
					print "$insts[$j]\n";
				}
			}
			$output = 1;
		}
	}

	if( $output == 0 ){
		print "$insts[$i]\n";
	}
}
