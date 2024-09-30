package studentConsulting.service.implement.user;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import studentConsulting.constant.SecurityConstants;
import studentConsulting.model.entity.department_field.DepartmentEntity;
import studentConsulting.model.entity.rating.RatingEntity;
import studentConsulting.model.entity.user.UserInformationEntity;
import studentConsulting.model.exception.CustomFieldErrorException;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.exception.FieldErrorDetail;
import studentConsulting.model.payload.dto.department_field.DepartmentDTO;
import studentConsulting.model.payload.dto.rating.RatingDTO;
import studentConsulting.model.payload.request.rating.CreateRatingRequest;
import studentConsulting.repository.communication.ConversationRepository;
import studentConsulting.repository.department_field.DepartmentRepository;
import studentConsulting.repository.question_answer.AnswerRepository;
import studentConsulting.repository.rating.RatingRepository;
import studentConsulting.repository.user.UserRepository;
import studentConsulting.service.interfaces.user.IUserRatingService;
import studentConsulting.specification.rating.RatingSpecification;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

@Service
public class UserRatingServiceImpl implements IUserRatingService {

    @Autowired
    private RatingRepository ratingRepository;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private DepartmentRepository departmentRepository;

    @Autowired
    private ConversationRepository conversationRepository;

    @Autowired
    private AnswerRepository answerRepository;

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

        boolean hasConsultantRole = userRepository.existsByUserIdAndRoleName(consultant.getId(), SecurityConstants.Role.TUVANVIEN);
        if (!hasConsultantRole) {
            errors.add(new FieldErrorDetail("role", "Người dùng không có vai trò tư vấn viên"));
            throw new CustomFieldErrorException(errors);
        }

        boolean consultantHasAnswered = answerRepository.hasConsultantAnsweredUserQuestions(user.getId(), request.getConsultantId());
        if (!consultantHasAnswered) {
            throw new ErrorException("Bạn chỉ có thể đánh giá tư vấn viên đã trả lời câu hỏi của bạn.");
        }

        boolean alreadyRated = ratingRepository
                .exists(RatingSpecification.hasUserAndConsultant(user.getId(), consultant.getId()));
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

    @Override
    public RatingDTO getRatingById(Integer ratingId, String email) {
        Optional<RatingEntity> ratingOpt = ratingRepository.findByIdAndUserAccountEmail(ratingId, email);
        if (!ratingOpt.isPresent()) {
            return null;
        }
        RatingEntity rating = ratingOpt.get();
        return mapToDTO(rating);
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
