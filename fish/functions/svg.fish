function svg
	dot -Tsvg -o /tmp/out.svg graph.dot; and open -a "Google Chrome" /tmp/out.svg
end
