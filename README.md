### DEPRECATED

Basic bot for responding to "I'm new, how do I learn to play Dota" posts on /r/dota2. Checks the new queue every 10 minutes, guesses whether it should respond to any posts (weights words and gets the post's average word weight), then responds with the contents of `reply_body`. There are some example posts in `Examples`, the bot should respond to the ones marked "correct", but not the rest.

Reddit API is managed w/ [Reddit](https://github.com/intolerable/reddit), you'll probably have to pull and build that if you want to get this working.

It's very basic right now, contributions are welcomed - make sure it compiles without warnings though.
