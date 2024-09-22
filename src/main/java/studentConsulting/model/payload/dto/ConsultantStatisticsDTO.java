package studentConsulting.model.payload.dto;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class ConsultantStatisticsDTO {
	private Integer totalQuestionsInDay; 
	private Integer totalForwardedQuestions;  
    private Integer totalDeletedQuestions;     
    private Integer totalAnswersGiven;  
    private Integer totalAnswerApproval;
    private Integer totalConsultantSchedule;
    private Integer totalApprovedPosts;        
    private Integer totalConversations;       
}
