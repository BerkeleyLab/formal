
Every GPU-node login session
----------------------------

1. Ensure `~/.bash_profile` is set up with all required environment variables, make
   sure $PYTHONPATH includes Mahesh's python
2. Ensure $FORMAL_DIR is on `rag-metadata` branch
3. Define $OUTPUT_DIR to point to place where .json file with RAG embeddings will be saved and then later read from
4. Write user prompt into file in $MODCON_DIR/AstraAI/AstraAI_fortran/user_prompt.txt
5. Start ollama server

```
salloc --nodes 1 --qos interactive --time 01:00:00 --constraint gpu --gpus 1 --account=project-num #get GPU node
source ~/.bash_profile #source profile again
source $MODCON_ENV/bin/activate
ollama serve &
```
where project-num is m2878 for the Pagoda project.

After changing Formal's RAG metadata
------------------------------------
Produce RAG embeddings

```
cd $MODCON_DIR/RAG
python3 extract_RAG_metadata_fortran.py --embed-model=all-minilm --code-dir=$FORMAL_DIR --out-dir=$OUTPUT_DIR
```

Execute RAG
-----------
Use RAG embeddings and user prompt to get response from LLM

```
cd $MODCON_DIR/AstraAI/AstraAI_fortran
python3 pr_watcher.py --llm-model=Qwen2.5-Coder-32B-Instruct-for-ollama --embed-model=all-minilm --rag-metadata-dir=$OUTPUT_DIR --top-k=10  --ollama-bin=/global/cfs/cdirs/m2957/nataraj2/Tools/ollama/bin/ollama --prompt-file=user_prompt.txt --terminal
```
