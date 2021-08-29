if (!defined $initial_reply_to && $prompting) {
	do {
		$_= $term->readline("Message-ID to be used as In-Reply-To for the first email? ",
			$initial_reply_to);
	} while (!defined $_);

	$initial_reply_to = $_;
	$initial_reply_to =~ s/(^\s+|\s+$)//g;
}

if (!$smtp_server) {
	foreach (qw( /usr/sbin/sendmail /usr/lib/sendmail )) {
		if (-x $_) {
			$smtp_server = $_;
			last;
		}
	}
	$smtp_server ||= 'localhost'; # could be 127.0.0.1, too... *shrug*
}