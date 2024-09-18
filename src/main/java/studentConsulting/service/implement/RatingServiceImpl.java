package studentConsulting.service.implement;

import java.time.LocalDate;
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
import studentConsulting.model.payload.dto.DepartmentDTO;
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
		
		System.out.println("Consultant Department ID: " + consultant.getAccount().getDepartment().getId());
		System.out.println("Selected Department ID: " + department.getId());

		if (!consultant.getAccount().getDepartment().getId().equals(department.getId())) {
			errors.add(new FieldErrorDetail("consultant", "Tư vấn viên không thuộc đơn vị tư vấn đã chọn"));
			throw new CustomFieldErrorException(errors);
		}

		boolean hasConsultantRole = userRepository.existsByUserIdAndRoleName(consultant.getId(), "ROLE_TUVANVIEN");

		if (!hasConsultantRole) {
			errors.add(new FieldErrorDetail("role", "Người dùng không có vai trò tư vấn viên"));
			throw new CustomFieldErrorException(errors);
		}
		
		boolean alreadyRated = ratingRepository.exists(RatingSpecification.hasUserAndConsultant(user.getId(), consultant.getId()));
        if (alreadyRated) {
            throw new ErrorException("Bạn đã đánh giá tư vấn viên này rồi.");
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
		rating.setSubmittedAt(LocalDate.now());

		RatingEntity savedRating = ratingRepository.save(rating);

		return mapToDTO(savedRating);
	}

	@Override
	public Page<RatingDTO> getRatingsByUser(String username, Integer departmentId, String consultantName,
			LocalDate startDate, LocalDate endDate, int page, int size, String sortBy, String sortDir) {
		Specification<RatingEntity> spec = Specification.where(RatingSpecification.hasUser(username));
		if (departmentId != null) {
			spec = spec.and(RatingSpecification.hasDepartment(departmentId));
		}

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
				.department(rating.getDepartment() != null
        		? new DepartmentDTO(
                        rating.getDepartment().getId(), 
                        rating.getDepartment().getName()
                    ) 
                    : null)				
				.userName(rating.getUser().getLastName() + " " + rating.getUser().getFirstName())
				.consultantName(rating.getConsultant().getLastName() + " " + rating.getConsultant().getFirstName())
				.generalSatisfaction(rating.getGeneralSatisfaction()).generalComment(rating.getGeneralComment())
				.expertiseKnowledge(rating.getExpertiseKnowledge()).expertiseComment(rating.getExpertiseComment())
				.attitude(rating.getAttitude()).attitudeComment(rating.getAttitudeComment())
				.responseSpeed(rating.getResponseSpeed()).responseSpeedComment(rating.getResponseSpeedComment())
				.understanding(rating.getUnderstanding()).understandingComment(rating.getUnderstandingComment())
				.submittedAt(rating.getSubmittedAt()).build();
	}
}
