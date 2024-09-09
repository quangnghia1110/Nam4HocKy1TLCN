package studentConsulting.service.implement;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.entity.departmentField.DepartmentEntity;
import studentConsulting.model.entity.feedback.RatingEntity;
import studentConsulting.model.exception.CustomFieldErrorException;
import studentConsulting.model.exception.FieldErrorDetail;
import studentConsulting.model.payload.dto.RatingDTO;
import studentConsulting.model.payload.request.rating.CreateRatingRequest;
import studentConsulting.repository.DepartmentRepository;
import studentConsulting.repository.RatingRepository;
import studentConsulting.repository.UserRepository;
import studentConsulting.service.IRatingService;

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

		// Kiểm tra sự tồn tại của đơn vị tư vấn (ConsultingUnitEntity)
		Optional<DepartmentEntity> consultingUnitOpt = departmentRepository.findById(request.getDepartmentId());
		if (!consultingUnitOpt.isPresent()) {
			errors.add(new FieldErrorDetail("department", "Phòng ban không tồn tại"));
		}
		if (!errors.isEmpty()) {
	        throw new CustomFieldErrorException(errors);
	    }

		UserInformationEntity consultant = consultantOpt.get();
		DepartmentEntity department = consultingUnitOpt.get();

		// Kiểm tra xem tư vấn viên có thuộc đơn vị tư vấn đã chọn hay không
		if (!consultant.getAccount().getDepartment().getId().equals(department.getId())) {
			errors.add(new FieldErrorDetail("consultant", "Tư vấn viên không thuộc đơn vị tư vấn đã chọn"));
			throw new CustomFieldErrorException(errors);
		}

		boolean hasConsultantRole = userRepository.existsByUserIdAndRoleName(consultant.getId(), "TUVANVIEN");

		// In ra kết quả kiểm tra vai trò
		System.out.println("Has consultant role: " + hasConsultantRole);

		if (!hasConsultantRole) {
			errors.add(new FieldErrorDetail("role", "Người dùng không có vai trò tư vấn viên"));
		}

		if (!errors.isEmpty()) {
			throw new CustomFieldErrorException(errors);
		}

		// Tạo đối tượng RatingEntity
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

		// Lưu đánh giá vào cơ sở dữ liệu
		RatingEntity savedRating = ratingRepository.save(rating);

		// Trả về DTO của Rating
		return mapToDTO(savedRating);
	}

	// Chuyển đổi từ RatingEntity sang RatingDTO
	private RatingDTO mapToDTO(RatingEntity rating) {
		return RatingDTO.builder().departmentId(rating.getDepartment().getId())
				.userName(rating.getUser().getLastName() + " " + rating.getUser().getFirstName())
				.consultantName(rating.getConsultant().getLastName() + " " + rating.getConsultant().getFirstName())
				.generalSatisfaction(rating.getGeneralSatisfaction()).generalComment(rating.getGeneralComment())
				.expertiseKnowledge(rating.getExpertiseKnowledge()).expertiseComment(rating.getExpertiseComment())
				.attitude(rating.getAttitude()).attitudeComment(rating.getAttitudeComment())
				.responseSpeed(rating.getResponseSpeed()).responseSpeedComment(rating.getResponseSpeedComment())
				.understanding(rating.getUnderstanding()).understandingComment(rating.getUnderstandingComment())
				.submittedAt(rating.getSubmittedAt()).build();
	}

	@Override
	public Page<RatingDTO> getRatingsByUserAndDepartmentAndConsultantName(UserInformationEntity user,
			Integer departmentId, String consultantName, Pageable pageable) {
		Page<RatingEntity> ratings = ratingRepository.findByUserAndDepartmentAndConsultantName(user, departmentId,
				consultantName, pageable);
		return ratings.map(this::mapToDTO);
	}

	@Override
	public Page<RatingDTO> getRatingsByUserAndDepartment(UserInformationEntity user, Integer departmentId,
			Pageable pageable) {
		Page<RatingEntity> ratings = ratingRepository.findByUserAndDepartment(user, departmentId, pageable);
		return ratings.map(this::mapToDTO);
	}

	@Override
	public Page<RatingDTO> searchRatingsByUserAndConsultantName(UserInformationEntity user, String consultantName,
			Pageable pageable) {
		Page<RatingEntity> ratings = ratingRepository.findByUserAndConsultantName(user, consultantName, pageable);
		return ratings.map(this::mapToDTO);
	}

	@Override
	public Page<RatingDTO> getAllRatingsByUser(UserInformationEntity user, Pageable pageable) {
		Page<RatingEntity> ratings = ratingRepository.findByUser(user, pageable);
		return ratings.map(this::mapToDTO);
	}

}
