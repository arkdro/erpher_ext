INCLUDE_DIR = include
#INCLUDE_DIR += ../proper/include ..
INCLUDES = $(INCLUDE_DIR:%=-I%)
SRC_DIR = src
EBIN_DIR := ebin
HTML_DOC_DIR = doc/html
ERLC_OPTS = +debug_info -DTEST
#ERLC_OPTS = +debug_info -DTEST -DPROPER -pa ../proper/ebin
ERLC := erlc $(ERLC_OPTS)
VSN=0.0.1
APP_NAME=erpher_ext
LICENSE=MIT

all: $(EBIN_DIR)
	$(ERLC) -W $(INCLUDES) -o $(EBIN_DIR) $(SRC_DIR)/*.erl
	cp $(SRC_DIR)/erpher_ext.app.src $(EBIN_DIR)/erpher_ext.app

clean:
	@rm -rvf $(EBIN_DIR)/* $(HTML_DOC_DIR)

tags: ctags etags

ctags:
	cd $(SRC_DIR) ; ctags -R . ../include 

etags:
	cd $(SRC_DIR) ; etags -R . ../include 

$(EBIN_DIR) :
	( test -d $(EBIN_DIR) || mkdir -p $(EBIN_DIR) )

dia:
	dialyzer \
		-Wrace_conditions \
		-Werror_handling \
		$(INCLUDES) \
		--src \
		-r $(SRC_DIR)

doc:
	erl -noshell -run edoc_run application "'$(APP_NAME)'" \
		'"."' \
		'[{dir,"$(HTML_DOC_DIR)"},{new, true},{hidden, true},{private, true},{def,[{vsn,"$(VSN)"}, {license, "(License: $(LICENSE))"}]}]'

.PHONY: clean ctags dia doc
