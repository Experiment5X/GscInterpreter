mergeSort(nums)
{
    if (nums.size <= 1)
        return nums;

    left = [];
    right = [];

    for (i = 0; i < (nums.size / 2); i++)
        append(left, nums[i]);

    for (i = nums.size / 2; i < nums.size; i++)
        append(right, nums[i]);

    leftS = mergeSort(left);
    rightS = mergeSort(right);

    leftIndex = 0;
    rightIndex = 0;

    // merge them together
    merged = [];
    while (leftIndex < leftS.size && rightIndex < rightS.size)
    {
        if (leftS[leftIndex] < rightS[rightIndex])
        {
            append(merged, leftS[leftIndex]);
            leftIndex++;
        }
        else
        {
            append(merged, rightS[rightIndex]);
            rightIndex++;
        }
    }

    // add the extras
    for (i = leftIndex; i < leftS.size; i++)
        append(merged, leftS[i]);

    for (i = rightIndex; i < rightS.size; i++)
        append(merged, rightS[i]);

    return merged;
}

main()
{
    nums = [7,2,4,7,6,18,4,9,14,43,3,2];
    sorted = mergeSort(nums);
    for (i = 0; i < sorted.size; i++)
        print(sorted[i]);
}