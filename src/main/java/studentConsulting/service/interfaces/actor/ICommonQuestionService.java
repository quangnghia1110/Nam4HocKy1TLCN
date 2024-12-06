package studentConsulting.service.interfaces.actor;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.web.multipart.MultipartFile;
import studentConsulting.model.entity.UserInformationEntity;
import studentConsulting.model.payload.dto.actor.CommonQuestionDTO;
import studentConsulting.model.payload.request.CommonQuestionRequest;

import java.security.Principal;
import java.time.LocalDate;

public interface ICommonQuestionService {

    public CommonQuestionDTO convertToCommonQuestion(Integer questionId, MultipartFile file, MultipartFile fileAnswer, Principal principal);

    public CommonQuestionDTO updateCommonQuestion(Integer commonQuestionId, MultipartFile file, MultipartFile fileAnswer,CommonQuestionRequest request, Principal principal);

    void deleteCommonQuestion(Integer id, UserInformationEntity user);

    CommonQuestionDTO getCommonQuestionById(Integer questionId, UserInformationEntity user);

    Page<CommonQuestionDTO> getCommonQuestionByRole(Integer departmentId, String title, LocalDate startDate, LocalDate endDate, Pageable pageable);

    public CommonQuestionDTO createCommonQuestion(CommonQuestionRequest request, MultipartFile file, MultipartFile fileAnswer, Principal principal);

    }
