package studentConsulting.service.interfaces.actor;

import org.springframework.data.domain.Page;
import studentConsulting.model.entity.UserInformationEntity;
import studentConsulting.model.payload.dto.actor.RatingDTO;
import studentConsulting.model.payload.request.CreateRatingRequest;

import java.time.LocalDate;

public interface IRatingService {

    RatingDTO createRating(CreateRatingRequest request, UserInformationEntity user);

    public Page<RatingDTO> getListRatingByRole(String email, Integer departmentId, String consultantName, LocalDate startDate, LocalDate endDate, int page, int size, String sortBy, String sortDir, boolean isAdmin, boolean isAdvisor, boolean isConsultant, Integer depId);

    RatingDTO getDetailRatingByRole(Integer ratingId, String email, Integer departmentId, boolean isAdmin, boolean isAdvisor, boolean isConsultant);

    RatingDTO getRatingByConsultantId(Integer consultantId, Integer userId);
}
