#!/bin/sh

GALAGO="/home/pv/galago/galago-5.7/core/target/appassembler/bin/galago"
INDEX="/home/pv/index/robust04"

PASSAGE_JSON="{
\"casefold\" : false,
\"requested\" : 1,
\"processingModel\":\"org.lemurproject.galago.core.retrieval.processing.RankedPassageModel\",
\"passageQuery\":true,
\"passageSize\": 50,
\"passageShift\" : 25,
\"queries\" : [
{
\"number\" : \" 301\",
\"text\" : \" international organized crime\"
}
]
}
"

echo ${PASSAGE_JSON} > tmp
## get passage results from galago and pull the docid, passage start, and passage end
RESULTS=`echo \`${GALAGO} batch-search --index=${INDEX} tmp\` | awk '
{
  split($0, words, " ")
  for (i=3; i <= length(words); i+=8) {
#  if (i % 8 ==3)
    printf("%s|%s|%s\n", words[i],words[i+4],words[i+5])
  }
}'`

for RESULT in ${RESULTS};
do
    DOC_ID=`echo ${RESULT} | cut -d'|' -f1`
    START=`echo ${RESULT} | cut -d'|' -f2`
    END=`echo ${RESULT} | cut -d'|' -f3`
    PASSAGE=`echo \`${GALAGO} doc --index=${INDEX} --id=${DOC_ID}\` | awk -v s=${START} -v e=${END} '
    {
      split($0, words, " ")
      for (i=s; i <= e; i++) {
        printf("%s\n", words[i])
      }
    }'`


    echo $DOC_ID $START $END
done
