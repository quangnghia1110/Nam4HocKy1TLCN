package studentConsulting.service.interfaces.user;

import org.springframework.data.domain.Page;
import studentConsulting.model.entity.user.UserInformationEntity;
import studentConsulting.model.payload.dto.RatingDTO;
import studentConsulting.model.payload.request.rating.CreateRatingRequest;

import java.time.LocalDate;

public interface IUserRatingService {

    RatingDTO createRating(CreateRatingRequest request, UserInformationEntity user);

    Page<RatingDTO> getRatingsByUser(String username, Integer departmentId, String consultantName,
                                     LocalDate startDate, LocalDate endDate, int page, int size, String sortBy, String sortDir);
}
