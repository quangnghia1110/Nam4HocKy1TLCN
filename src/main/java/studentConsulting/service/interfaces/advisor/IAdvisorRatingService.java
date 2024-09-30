package studentConsulting.service.interfaces.advisor;

import org.springframework.data.domain.Page;
import studentConsulting.model.payload.dto.rating.RatingDTO;

import java.time.LocalDate;

public interface IAdvisorRatingService {
    Page<RatingDTO> getRatingsByDepartment(Integer departmentId, String consultantName, LocalDate startDate, LocalDate endDate, int page, int size, String sortBy, String sortDir);
}
