package studentConsulting.service;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.payload.dto.RatingDTO;
import studentConsulting.model.payload.request.rating.CreateRatingRequest;

public interface IRatingService {
	public RatingDTO createRating(CreateRatingRequest request, UserInformationEntity user);

	public Page<RatingDTO> getRatingsByUserAndDepartmentAndConsultantName(UserInformationEntity user,
			Integer departmentId, String consultantName, Pageable pageable);

	public Page<RatingDTO> getRatingsByUserAndDepartment(UserInformationEntity user, Integer departmentId,
			Pageable pageable);

	public Page<RatingDTO> searchRatingsByUserAndConsultantName(UserInformationEntity user, String consultantName,
			Pageable pageable);

	public Page<RatingDTO> getAllRatingsByUser(UserInformationEntity user, Pageable pageable);

}
