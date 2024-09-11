package studentConsulting.service.implement;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;

import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.entity.departmentField.DepartmentEntity;
import studentConsulting.model.entity.feedback.RatingEntity;
import studentConsulting.model.exception.CustomFieldErrorException;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.exception.FieldErrorDetail;
import studentConsulting.model.payload.dto.RatingDTO;
import studentConsulting.model.payload.request.rating.CreateRatingRequest;
import studentConsulting.repository.DepartmentRepository;
import studentConsulting.repository.RatingRepository;
import studentConsulting.repository.UserRepository;
import studentConsulting.service.IRatingService;
import studentConsulting.specification.RatingSpecification;

@Service
public class RatingServiceImpl implements IRatingService {

	@Autowired
	private RatingRepository ratingRepository;

	@Autowired
	private UserRepository userRepository;

	@Autowired
	private DepartmentRepository departmentRepository;

	@Override
	public RatingDTO createRating(CreateRatingRequest request, UserInformationEntity user) {
		List<FieldErrorDetail> errors = new ArrayList<>();

		Optional<UserInformationEntity> consultantOpt = userRepository.findById(request.getConsultantId());
		if (!consultantOpt.isPresent()) {
			errors.add(new FieldErrorDetail("consultant", "Tư vấn viên không tồn tại"));
		}

		Optional<DepartmentEntity> consultingUnitOpt = departmentRepository.findById(request.getDepartmentId());
		if (!consultingUnitOpt.isPresent()) {
			errors.add(new FieldErrorDetail("department", "Phòng ban không tồn tại"));
		}

		if (!errors.isEmpty()) {
			throw new CustomFieldErrorException(errors);
		}

		UserInformationEntity consultant = consultantOpt.get();
		DepartmentEntity department = consultingUnitOpt.get();

		if (!consultant.getAccount().getDepartment().getId().equals(department.getId())) {
			errors.add(new FieldErrorDetail("consultant", "Tư vấn viên không thuộc đơn vị tư vấn đã chọn"));
			throw new CustomFieldErrorException(errors);
		}

		boolean hasConsultantRole = userRepository.existsByUserIdAndRoleName(consultant.getId(), "TUVANVIEN");

		if (!hasConsultantRole) {
			errors.add(new FieldErrorDetail("role", "Người dùng không có vai trò tư vấn viên"));
			throw new CustomFieldErrorException(errors);
		}

		RatingEntity rating = new RatingEntity();
		rating.setUser(user);
		rating.setConsultant(consultant);
		rating.setDepartment(department);
		rating.setGeneralSatisfaction(request.getGeneralSatisfaction());
		rating.setGeneralComment(request.getGeneralComment());
		rating.setExpertiseKnowledge(request.getExpertiseKnowledge());
		rating.setExpertiseComment(request.getExpertiseComment());
		rating.setAttitude(request.getAttitude());
		rating.setAttitudeComment(request.getAttitudeComment());
		rating.setResponseSpeed(request.getResponseSpeed());
		rating.setResponseSpeedComment(request.getResponseSpeedComment());
		rating.setUnderstanding(request.getUnderstanding());
		rating.setUnderstandingComment(request.getUnderstandingComment());
		rating.setSubmittedAt(LocalDateTime.now());

		RatingEntity savedRating = ratingRepository.save(rating);

		return mapToDTO(savedRating);
	}

	@Override
	public Page<RatingDTO> getRatingsByUser(String username, Integer departmentId, String consultantName, int page, int size, String sortBy, String sortDir) {
		UserInformationEntity user = userRepository.findByAccountUsername(username)
				.orElseThrow(() -> new ErrorException("Người dùng không tồn tại"));

		Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

		Specification<RatingEntity> spec = Specification.where(RatingSpecification.hasUser(user));

		if (departmentId != null) {
			spec = spec.and(RatingSpecification.hasDepartmentId(departmentId));
		}

		if (consultantName != null && !consultantName.isEmpty()) {
			spec = spec.and(RatingSpecification.hasConsultantName(consultantName));
		}

		Page<RatingEntity> ratingEntities = ratingRepository.findAll(spec, pageable);

		if (ratingEntities.isEmpty()) {
			throw new ErrorException("Không tìm thấy đánh giá nào.");
		}

		return ratingEntities.map(this::mapToDTO);
	}

	private RatingDTO mapToDTO(RatingEntity rating) {
		return RatingDTO.builder()
				.departmentId(rating.getDepartment().getId())
				.userName(rating.getUser().getLastName() + " " + rating.getUser().getFirstName())
				.consultantName(rating.getConsultant().getLastName() + " " + rating.getConsultant().getFirstName())
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

