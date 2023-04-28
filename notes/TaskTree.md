# Task Tree

A **task** is a specific goal for the agent.

A **task tree** is a task and a list of subtasks such that once all of the subtasks are completed then the parent task is also completed.

A **task zipper** is a zipper into a task tree i.e. an in-progress task tree. The zipper's path corresponds to the heirarchy of in-progress tasks, and the zipper's tree corresponds to the next task to do.

## Task Tree Grower Algorithm

Each step of the algorithm goes like this:

The current zipper has this form:
```
TASK: do thing 1
TASK: do thing 2
TASK: do thing 3
  TASK: do thing 3.1
    TASK: do thing 3.1.1
```
Where task 3.1.1 is a prototype (doesn't have children). The agent processes this step, yielding one of the following results:
- DONE: the agent can do this task atomically, so mark as done and record the result
- DESTRUCT: the agent can break this task into subtasks, so conver the task to a task with those subtasks as prototypes. Then do the subtasks in order.

