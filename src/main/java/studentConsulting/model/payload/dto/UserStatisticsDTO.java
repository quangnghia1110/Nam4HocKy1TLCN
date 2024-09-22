package studentConsulting.model.payload.dto;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class UserStatisticsDTO {
	private long totalQuestions;
	private long questionsOver500Views;
	private long totalAppointments;
	private long totalRatings;
}
