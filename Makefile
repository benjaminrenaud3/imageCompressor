##
## EPITECH PROJECT, 207
## Makefile
## File description:
## Makefile
##

NAME	=	imgCompressor

SRC	=	app/Main.hs		\

all:	$(NAME)

$(NAME): $(SRC)
	stack build --copy-bins --local-bin-path .

clean:
	stack clean
	rm .stack-work ImgCompressor.cabal -rf

fclean:	clean
	$(RM) $(NAME)

re:	fclean all