# Taken from https://pythonprojects.io/scatter-plot-with-matplotlib

import matplotlib.pyplot as plt

def make_scatter_plot():
    exam_scores = [50, 35, 90, 63, 85, 99, 78]
    hours_studying = [2, 1, 7, 4, 6, 9, 5]

    # label the x axis with Hours Spent Studying
    plt.xlabel('Hours Spent Studying')

    # label the y axis with Exam Scores
    plt.ylabel('Exam Scores')

    # we need to tell matplotlib what type of graph we'd like to use
    plt.scatter(hours_studying, exam_scores, c='b')

    # show our graph
    plt.show()

make_scatter_plot()
