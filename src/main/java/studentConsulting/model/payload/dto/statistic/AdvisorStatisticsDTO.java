package studentConsulting.model.payload.dto.statistic;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class AdvisorStatisticsDTO {
    private Integer totalQuestionsInDay;
    private Integer totalForwardedQuestions;
    private Integer totalDeletedQuestions;
    private Integer totalAnswersGiven;
    private Integer totalAnswerApproval;
    private Integer totalConsultantSchedule;
    //    private Integer totalApprovedPosts;
    private Integer totalConversations;
    private Integer totalRatings;
    private Integer totalCommonQuestions;
    private Integer totalConsultants;
}

