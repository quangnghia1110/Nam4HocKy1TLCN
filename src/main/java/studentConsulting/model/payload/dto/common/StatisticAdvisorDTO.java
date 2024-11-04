package studentConsulting.model.payload.dto.common;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class StatisticAdvisorDTO {
    private Integer totalQuestions;
    private Integer totalForwardedQuestions;
    private Integer totalDeletedQuestions;
    private Integer totalAnswersGiven;
    private Integer totalAnswerApproval;
    private Integer totalConfirmedConsultantSchedule;
    private Integer totalApprovedPosts;
    private Integer totalConversations;
    private Integer totalRatings;
    private Integer totalUniqueUsersAdvisedByMessages;

    private Integer totalCommonQuestions;
    private Integer totalConsultants;
}

