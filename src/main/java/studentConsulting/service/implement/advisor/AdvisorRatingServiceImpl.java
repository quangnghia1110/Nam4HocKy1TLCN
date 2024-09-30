package studentConsulting.service.implement.advisor;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import studentConsulting.model.entity.rating.RatingEntity;
import studentConsulting.model.payload.dto.department_field.DepartmentDTO;
import studentConsulting.model.payload.dto.rating.RatingDTO;
import studentConsulting.repository.rating.RatingRepository;
import studentConsulting.service.interfaces.advisor.IAdvisorRatingService;
import studentConsulting.specification.rating.RatingSpecification;

import java.time.LocalDate;

@Service
public class AdvisorRatingServiceImpl implements IAdvisorRatingService {

    @Autowired
    private RatingRepository ratingRepository;

    @Override
    public Page<RatingDTO> getRatingsByDepartment(Integer departmentId, String consultantName, LocalDate startDate, LocalDate endDate, int page, int size, String sortBy, String sortDir) {
        Specification<RatingEntity> spec = Specification.where(RatingSpecification.hasDepartment(departmentId));

        if (consultantName != null && !consultantName.isEmpty()) {
            spec = spec.and(RatingSpecification.hasConsultantName(consultantName));
        }

        if (startDate != null && endDate != null) {
            spec = spec.and(RatingSpecification.hasExactDateRange(startDate, endDate));
        } else if (startDate != null) {
            spec = spec.and(RatingSpecification.hasExactStartDate(startDate));
        } else if (endDate != null) {
            spec = spec.and(RatingSpecification.hasDateBefore(endDate));
        }

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));
        Page<RatingEntity> ratingEntities = ratingRepository.findAll(spec, pageable);

        return ratingEntities.map(this::mapToDTO);
    }

    private RatingDTO mapToDTO(RatingEntity rating) {
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
