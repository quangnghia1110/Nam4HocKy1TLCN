package studentConsulting.service.implement;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.LocalDate;
import java.util.Map;
import java.util.Optional;

import javax.transaction.Transactional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import com.cloudinary.Cloudinary;

import studentConsulting.model.entity.questionAnswer.AnswerEntity;
import studentConsulting.model.entity.questionAnswer.CommonQuestionEntity;
import studentConsulting.model.entity.questionAnswer.QuestionEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.CommonQuestionDTO;
import studentConsulting.model.payload.request.commonQuestion.UpdateCommonQuestionRequest;
import studentConsulting.repository.AnswerRepository;
import studentConsulting.repository.CommonQuestionRepository;
import studentConsulting.repository.QuestionRepository;
import studentConsulting.service.ICommonQuestionService;
import studentConsulting.specification.CommonQuestionSpecification;

@Service
public class CommonQuestionServiceImpl implements ICommonQuestionService {

	@Autowired
	private Cloudinary cloudinary;
	
    @Autowired
    private CommonQuestionRepository commonQuestionRepository;

    @Autowired
    private QuestionRepository questionRepository;
    
    @Autowired
    private AnswerRepository answerRepository;

    @Override
    public Page<CommonQuestionDTO> getCommonQuestionsWithFilters(Integer departmentId,String title, LocalDate startDate, LocalDate endDate, Pageable pageable) {
        
        Specification<CommonQuestionEntity> spec = Specification.where(null);

        if (departmentId != null) {
            spec = spec.and(CommonQuestionSpecification.hasDepartment(departmentId));
        }

        if (title != null && !title.isEmpty()) {
            spec = spec.and(CommonQuestionSpecification.hasTitle(title));
        }

        if (startDate != null && endDate != null) {
            spec = spec.and(CommonQuestionSpecification.hasExactDateRange(startDate, endDate));
        } else if (startDate != null) {
            spec = spec.and(CommonQuestionSpecification.hasExactStartDate(startDate));
        } else if (endDate != null) {
            spec = spec.and(CommonQuestionSpecification.hasDateBefore(endDate));
        }

        Page<CommonQuestionEntity> commonQuestions = commonQuestionRepository.findAll(spec, pageable);
        return commonQuestions.map(this::mapToDTO);
    }

    private CommonQuestionDTO mapToDTO(CommonQuestionEntity question) {
        return CommonQuestionDTO.builder()
        	.commonQuestionId(question.getId())
            .department(CommonQuestionDTO.DepartmentDTO.builder()
                .id(question.getDepartment().getId())
                .name(question.getDepartment().getName())
                .build())
            .field(CommonQuestionDTO.FieldDTO.builder()
                .id(question.getField().getId())
                .name(question.getField().getName())
                .build())
            .roleAsk(CommonQuestionDTO.RoleAskDTO.builder()
                .id(question.getRoleAsk().getId())
                .name(question.getRoleAsk().getName())
                .build())
            .title(question.getTitle())
            .content(question.getContent())
            .fileName(question.getFileName())
            .answerTitle(question.getAnswerTitle())
            .answerContent(question.getAnswerContent())
            .answerUserEmail(question.getAnswerUserEmail())
            .answerUserFirstname(question.getUser().getFirstName())
            .answerUserLastname(question.getUser().getLastName())
            .answerCreatedAt(question.getAnswerCreatedAt())
            .views(question.getViews())
            .createdAt(question.getCreatedAt())
            .askerFirstname(question.getUser().getFirstName())
            .askerLastname(question.getUser().getLastName())
            .build();
    }

    @Override
    @Transactional
    public CommonQuestionDTO convertToCommonQuestion(Integer questionId) {
        QuestionEntity question = questionRepository.findById(questionId)
                .orElseThrow(() -> new RuntimeException("Câu hỏi không tồn tại"));

        CommonQuestionEntity commonQuestion = new CommonQuestionEntity();
        commonQuestion.setTitle(question.getTitle());
        commonQuestion.setContent(question.getContent());
        commonQuestion.setFileName(question.getFileName());
        commonQuestion.setCreatedAt(question.getCreatedAt());
        commonQuestion.setViews(question.getViews());
        commonQuestion.setDepartment(question.getDepartment());
        commonQuestion.setField(question.getField());
        commonQuestion.setRoleAsk(question.getRoleAsk());
        commonQuestion.setUser(question.getUser());
        
        if (question.getUser() != null) {
            commonQuestion.setAskerFirstname(question.getUser().getFirstName());
            commonQuestion.setAskerLastname(question.getUser().getLastName());
        } else {
            commonQuestion.setAskerFirstname("Unknown");
            commonQuestion.setAskerLastname("Unknown");
        }

        Optional<AnswerEntity> firstAnswer = answerRepository.findFirstAnswerByQuestionId(question.getId());
        if (firstAnswer.isPresent()) {
            AnswerEntity answer = firstAnswer.get();
            commonQuestion.setAnswerContent(answer.getContent());
            
            if (answer.getUser() != null && answer.getUser().getAccount() != null) {
                commonQuestion.setAnswerUserEmail(answer.getUser().getAccount().getEmail());
                commonQuestion.setAnswerUserFirstname(answer.getUser().getFirstName());
                commonQuestion.setAnswerUserLastname(answer.getUser().getLastName());
            } else {
                commonQuestion.setAnswerUserEmail("unknown@example.com");
                commonQuestion.setAnswerUserFirstname("Unknown");
                commonQuestion.setAnswerUserLastname("Unknown");
            }
            commonQuestion.setAnswerCreatedAt(answer.getCreatedAt());
            commonQuestion.setAnswerTitle(answer.getTitle());
        }

        CommonQuestionEntity savedCommonQuestion = commonQuestionRepository.save(commonQuestion);

        return mapToDTO(savedCommonQuestion);
    }
    
    @Override
    @Transactional
    public CommonQuestionDTO updateCommonQuestion(Integer commonQuestionId, Integer departmentId, UpdateCommonQuestionRequest request) {
        CommonQuestionEntity existingCommonQuestion = commonQuestionRepository.findById(commonQuestionId)
                .orElseThrow(() -> new RuntimeException("Câu hỏi tổng hợp không tồn tại"));

        if (!existingCommonQuestion.getDepartment().getId().equals(departmentId)) {
            throw new RuntimeException("Không có quyền cập nhật câu hỏi này vì nó không thuộc phòng ban của bạn.");
        }

        existingCommonQuestion.setTitle(request.getTitle());
        existingCommonQuestion.setContent(request.getContent());
        if (request.getFileName() != null && !request.getFileName().isEmpty()) {
            String fileName = saveFile(request.getFileName());
            existingCommonQuestion.setFileName(fileName);
        }        


        existingCommonQuestion.setAnswerTitle(request.getAnswerTitle());
        existingCommonQuestion.setAnswerContent(request.getAnswerContent());

        CommonQuestionEntity updatedCommonQuestion = commonQuestionRepository.save(existingCommonQuestion);

        return mapToDTO(updatedCommonQuestion);
    }
    
    @Override
    @Transactional
    public void deleteCommonQuestion(Integer id, Integer departmentId) {
        Optional<CommonQuestionEntity> commonQuestionOpt = commonQuestionRepository.findByIdAndDepartmentId(id, departmentId);
        if (!commonQuestionOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy câu hỏi tổng hợp.");
        }

        commonQuestionRepository.delete(commonQuestionOpt.get());
    }


    private String saveFile(MultipartFile file) {
	    try {
	        String fileType = file.getContentType();

	        if (fileType != null && fileType.startsWith("image")) {
	            Map<String, Object> uploadResult = cloudinary.uploader().upload(file.getBytes(), Map.of());
	            String fileUrl = (String) uploadResult.get("url");
	            return fileUrl; 
	        } else {
	            String uploadDir = "D:\\HCMUTE-K21\\DoAnGitHub\\Nam4HocKy1TLCN\\upload";
	            Path uploadPath = Paths.get(uploadDir);

	            if (!Files.exists(uploadPath)) {
	                Files.createDirectories(uploadPath);
	            }

	            String originalFileName = file.getOriginalFilename();
	            Path filePath = uploadPath.resolve(originalFileName);

	            Files.write(filePath, file.getBytes());

	            return filePath.toString();  
	        }
	    } catch (IOException e) {
	        throw new RuntimeException("Could not store the file. Error: " + e.getMessage());
	    }
	}
    
}