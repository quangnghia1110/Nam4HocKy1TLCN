package studentConsulting.service;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.payload.dto.RatingDTO;
import studentConsulting.model.payload.request.rating.CreateRatingRequest;

public interface IRatingService {

    RatingDTO createRating(CreateRatingRequest request, UserInformationEntity user);

    Page<RatingDTO> getRatingsByUser(String username, Integer departmentId, String consultantName, int page, int size, String sortBy, String sortDir);
}
