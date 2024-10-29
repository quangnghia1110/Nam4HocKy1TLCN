package studentConsulting.model.payload.mapper.actor;

import org.springframework.stereotype.Component;
import studentConsulting.model.entity.rating.RatingEntity;
import studentConsulting.model.payload.dto.department_field.DepartmentDTO;
import studentConsulting.model.payload.dto.rating.RatingDTO;

@Component
public class RatingMapper {
    public RatingDTO mapToDTO(RatingEntity rating) {
        return RatingDTO.builder()
                .id(rating.getId())
                .department(rating.getDepartment() != null
                        ? new DepartmentDTO(rating.getDepartment().getId(), rating.getDepartment().getName())
                        : null)
                .user(RatingDTO.UserDTO.builder()
                        .id(rating.getUser().getId())
                        .name(rating.getUser().getLastName() + " " + rating.getUser().getFirstName())
                        .build())
                .consultant(RatingDTO.UserDTO.builder()
                        .id(rating.getConsultant().getId())
                        .name(rating.getConsultant().getLastName() + " " + rating.getConsultant().getFirstName())
                        .build())
                .generalSatisfaction(rating.getGeneralSatisfaction())
                .generalComment(rating.getGeneralComment())
                .expertiseKnowledge(rating.getExpertiseKnowledge())
                .expertiseComment(rating.getExpertiseComment())
                .attitude(rating.getAttitude())
                .attitudeComment(rating.getAttitudeComment())
                .responseSpeed(rating.getResponseSpeed())
                .responseSpeedComment(rating.getResponseSpeedComment())
                .understanding(rating.getUnderstanding())
                .understandingComment(rating.getUnderstandingComment())
                .submittedAt(rating.getSubmittedAt())
                .build();
    }
}
