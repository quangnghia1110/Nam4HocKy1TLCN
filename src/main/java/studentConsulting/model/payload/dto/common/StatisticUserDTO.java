package studentConsulting.model.payload.dto.common;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class StatisticUserDTO {
    private Integer totalQuestions;
    private Integer questionsOver500Views;
    private Integer totalAppointments;
    private Integer totalRatings;
}
