package studentConsulting.model.payload.dto.actor;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDate;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class RatingDTO {
    private Integer id;

    private DepartmentDTO department;
    private UserDTO user;
    private UserDTO consultant;

    private int generalSatisfaction;
    private String generalComment;

    private int expertiseKnowledge;
    private String expertiseComment;

    private int attitude;
    private String attitudeComment;

    private int responseSpeed;
    private String responseSpeedComment;

    private int understanding;
    private String understandingComment;

    private LocalDate submittedAt;

    @Data
    @Builder
    public static class UserDTO {
        private Integer id;
        private String name;
        public UserDTO() {
        }
        public UserDTO(Integer id, String name) {
            this.id = id;
            this.name = name;
        }
    }
}

