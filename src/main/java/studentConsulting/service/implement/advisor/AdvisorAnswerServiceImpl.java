package studentConsulting.service.implement.advisor;

import com.cloudinary.Cloudinary;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import studentConsulting.model.entity.question_answer.AnswerEntity;
import studentConsulting.model.entity.question_answer.QuestionEntity;
import studentConsulting.model.exception.CustomFieldErrorException;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.exception.FieldErrorDetail;
import studentConsulting.model.payload.dto.question_answer.AnswerDTO;
import studentConsulting.model.payload.request.question_answer.ReviewAnswerRequest;
import studentConsulting.model.payload.request.question_answer.UpdateAnswerRequest;
import studentConsulting.repository.question_answer.AnswerRepository;
import studentConsulting.repository.question_answer.QuestionRepository;
import studentConsulting.repository.user.RoleConsultantRepository;
import studentConsulting.repository.user.UserRepository;
import studentConsulting.service.implement.common.CommonFileStorageServiceImpl;
import studentConsulting.service.interfaces.advisor.IAdvisorAnswerService;
import studentConsulting.service.interfaces.common.ICommonExcelService;
import studentConsulting.service.interfaces.common.ICommonPdfService;
import studentConsulting.specification.question_answer.AnswerSpecification;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

@Service
public class AdvisorAnswerServiceImpl implements IAdvisorAnswerService {

    @Autowired
    private Cloudinary cloudinary;

    @Autowired
    private AnswerRepository answerRepository;

    @Autowired
    private QuestionRepository questionRepository;

    @Autowired
    private RoleConsultantRepository roleConsultantRepository;

    @Autowired
    private UserRepository userInformationRepository;

    @Autowired
    private CommonFileStorageServiceImpl fileStorageService;

    @Autowired
    private ICommonPdfService pdfService;

    @Autowired
    private ICommonExcelService excelService;

    @Override
    public AnswerDTO reviewAnswer(ReviewAnswerRequest request) {
        List<FieldErrorDetail> errors = new ArrayList<>();

        Optional<AnswerEntity> answerOpt = answerRepository.findFirstAnswerByQuestionId(request.getQuestionId());
        if (answerOpt.isEmpty()) {
            errors.add(new FieldErrorDetail("answerId", "Câu trả lời không tồn tại."));
            throw new CustomFieldErrorException(errors);
        }

        AnswerEntity answer = answerOpt.get();
        if (answer.getStatusAnswer() != null && answer.getStatusAnswer()) {
            throw new ErrorException("Câu trả lời này đã được duyệt và không thể kiểm duyệt lại");
        }

        String fileName = null;
        if (request.getFile() != null && !request.getFile().isEmpty()) {
            fileName = fileStorageService.saveFile(request.getFile());
            answer.setFile(fileName);
        }

        answer.setContent(request.getContent());
        answer.setStatusAnswer(true);

        Optional<QuestionEntity> questionOpt = questionRepository.findById(request.getQuestionId());
        if (questionOpt.isEmpty()) {
            errors.add(new FieldErrorDetail("questionId", "Câu hỏi không tồn tại với ID: " + request.getQuestionId()));
        }

        QuestionEntity question = questionOpt.get();
        question.setStatusApproval(true);
        questionRepository.save(question);
        AnswerEntity reviewedAnswer = answerRepository.save(answer);

        return mapToAnswerDTO(reviewedAnswer);
    }

    public AnswerDTO mapToAnswerDTO(AnswerEntity answer) {
        return AnswerDTO.builder()
                .answerId(answer.getId())
                .questionId(answer.getQuestion().getId())
                .roleConsultantId(answer.getRoleConsultant().getId())
                .userId(answer.getUser().getId())
                .title(answer.getTitle())
                .content(answer.getContent())
                .file(answer.getFile())
                .createdAt(answer.getCreatedAt())
                .statusApproval(answer.getStatusApproval())
                .statusAnswer(answer.getStatusAnswer())
                .build();
    }

    @Override
    public Page<AnswerDTO> getApprovedAnswersByDepartmentWithFilters(Optional<Integer> departmentId, LocalDate startDate, LocalDate endDate, int page, int size, String sortBy, String sortDir) {
        Specification<AnswerEntity> spec = Specification.where(AnswerSpecification.hasApprovalStatus(true));

        if (departmentId.isPresent()) {
            spec = spec.and(AnswerSpecification.hasDepartment(departmentId.get()));
        }

        if (startDate != null && endDate != null) {
            spec = spec.and(AnswerSpecification.hasExactDateRange(startDate, endDate));
        } else if (startDate != null) {
            spec = spec.and(AnswerSpecification.hasExactStartDate(startDate));
        } else if (endDate != null) {
            spec = spec.and(AnswerSpecification.hasDateBefore(endDate));
        }

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));
        return answerRepository.findAll(spec, pageable).map(this::mapToAnswerDTO);
    }

    @Override
    public Page<AnswerDTO> getAllAnswersByDepartmentWithFilters(Optional<Integer> departmentId, LocalDate startDate, LocalDate endDate, int page, int size, String sortBy, String sortDir) {
        Specification<AnswerEntity> spec = Specification.where(null);

        if (departmentId.isPresent()) {
            spec = spec.and(AnswerSpecification.hasDepartment(departmentId.get()));
        }

        if (startDate != null && endDate != null) {
            spec = spec.and(AnswerSpecification.hasExactDateRange(startDate, endDate));
        } else if (startDate != null) {
            spec = spec.and(AnswerSpecification.hasExactStartDate(startDate));
        } else if (endDate != null) {
            spec = spec.and(AnswerSpecification.hasDateBefore(endDate));
        }

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));
        return answerRepository.findAll(spec, pageable).map(this::mapToAnswerDTO);
    }

    @Override
    public AnswerDTO updateAnswer(Integer answerId, UpdateAnswerRequest request) {
        AnswerEntity existingAnswer = answerRepository.findById(answerId)
                .orElseThrow(() -> new ErrorException("Câu trả lời không tồn tại"));

        existingAnswer.setTitle(request.getTitle());
        existingAnswer.setContent(request.getContent());
        existingAnswer.setStatusApproval(request.getStatusApproval());
        existingAnswer.setStatusAnswer(request.getStatusAnswer());

        if (request.getFile() != null && !request.getFile().isEmpty()) {
            String fileName = request.getFile().getOriginalFilename();
            existingAnswer.setFile(fileName); // Implement file save logic
        }

        AnswerEntity updatedAnswer = answerRepository.save(existingAnswer);
        return mapToAnswerDTO(updatedAnswer);
    }

    @Override
    public AnswerDTO updateAnswerByDepartment(Integer answerId, UpdateAnswerRequest request, Integer departmentId) {
        AnswerEntity existingAnswer = answerRepository.findByIdAndDepartmentId(answerId, departmentId)
                .orElseThrow(() -> new ErrorException("Câu trả lời không tồn tại trong bộ phận của bạn"));

        existingAnswer.setTitle(request.getTitle());
        existingAnswer.setContent(request.getContent());
        existingAnswer.setStatusApproval(request.getStatusApproval());
        existingAnswer.setStatusAnswer(request.getStatusAnswer());

        if (request.getFile() != null && !request.getFile().isEmpty()) {
            String fileName = request.getFile().getOriginalFilename();
            existingAnswer.setFile(fileName); // Implement file save logic
        }

        AnswerEntity updatedAnswer = answerRepository.save(existingAnswer);
        return mapToAnswerDTO(updatedAnswer);
    }

    @Override
    public void deleteAnswer(Integer id) {
        answerRepository.deleteById(id);
    }

    @Override
    public void deleteAnswerByDepartment(Integer id, Integer departmentId) {
        AnswerEntity answer = answerRepository.findByIdAndDepartmentId(id, departmentId)
                .orElseThrow(() -> new ErrorException("Câu trả lời không tồn tại trong bộ phận của bạn"));
        answerRepository.delete(answer);
    }

    @Override
    public AnswerDTO getAnswerById(Integer answerId) {
        AnswerEntity answer = answerRepository.findById(answerId)
                .orElseThrow(() -> new ErrorException("Câu trả lời không tồn tại"));
        return mapToAnswerDTO(answer);
    }

    @Override
    public AnswerDTO getAnswerByIdAndDepartment(Integer answerId, Integer departmentId) {
        AnswerEntity answer = answerRepository.findByIdAndDepartmentId(answerId, departmentId)
                .orElseThrow(() -> new ErrorException("Câu trả lời không tồn tại trong bộ phận của bạn"));
        return mapToAnswerDTO(answer);
    }
}